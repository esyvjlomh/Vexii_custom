package vexiiriscv.test

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.Global
import vexiiriscv.Global.{COMMIT, HART_COUNT, TRAP}
import vexiiriscv.decode.{Decode, DecodePipelinePlugin, DecoderPlugin}
import vexiiriscv.execute._
import vexiiriscv.execute.lsu._
import vexiiriscv.fetch.{Fetch, FetchPipelinePlugin}
import vexiiriscv.misc.{PipelineBuilderPlugin, PrivilegedPlugin, TrapPlugin}
import vexiiriscv.prediction.{BtbPlugin, LearnCmd, LearnPlugin}
import vexiiriscv.regfile.{RegFileWrite, RegFileWriter, RegFileWriterService}
import vexiiriscv.riscv.{Const, Riscv}
import vexiiriscv.schedule.{DispatchPlugin, FlushCmd, ReschedulePlugin}

import scala.collection.mutable.ArrayBuffer

/**
  * This plugin is here to ease and "standardise" the way a simulation can look at a VexiiRiscv core and figure out what
  * it is doing. It also generate a large set of easy to read signals that can be read in a waveform.
  *
  * All the "Proxy" are there to reduce the overhead of reading hardware signals in a SpinalHDL simulation.
  */
//  这个插件旨在简化和“标准化”仿真观察 VexiiRiscv 核心的方式，并了解它在做什么。
//  它还生成一组易于读取的信号，这些信号可以在波形中读取。
//  所有 “Proxy” 都用于减少在 SpinalHDL 仿真中读取硬件信号的开销。
class WhiteboxerPlugin(withOutputs : Boolean) extends FiberPlugin{
  //  WhiteboxerPlugin 类定义，继承自 FiberPlugin，并接受一个布尔值参数 withOutputs，
  //  指示是否将内部信号输出到物理输出引脚，以便在仿真器中观察。FiberPlugin 是一种插件类型。

  val logic = during setup new Logic()
  //  在 setup 阶段创建 Logic 实例。
  //  during setup 声明这段代码会在处理器搭建完成之后（setup）执行。
  //  new Logic() 创建一个 Logic 实例。

  class Logic extends Area{
    //  Logic 类，是插件的核心逻辑。
    val pbp = host[PipelineBuilderPlugin]
    //  获取 PipelineBuilderPlugin 插件的实例，用于构建流水线。
    val buildBefore = retains(pbp.elaborationLock)
    //  获取 pbp.elaborationLock，用来同步。
    awaitBuild()
    //  等待构建完成。

    def wrap[T <: Data](that: T): T = {
      //  wrap 方法，用于对信号进行封装，使其在仿真中可访问。
      val buffered = CombInit(that).simPublic // 组合逻辑缓冲+仿真可见
      //  使用 CombInit 将信号转换为组合逻辑，并使用 simPublic 使其在仿真中可见。
      if(withOutputs) out(buffered)           // 可选物理输出
      //  如果 withOutputs 为 true，则将信号输出到物理引脚。
      buffered
    }

    val fpp = host[FetchPipelinePlugin]
    //  获取 FetchPipelinePlugin 插件的实例。
    val dpp = host[DecodePipelinePlugin]
    //  获取 DecodePipelinePlugin 插件的实例。
    val fetch = new Area {
      //  Fetch 相关的信号集合。
      val c = fpp.fetch(0)
      //  获取 FetchPipelinePlugin 中第一个 FetchStage 的控制信号。
      val fire = wrap(c.down.isFiring)
      //  获取 fetch stage 的 isFiring 信号，并用 wrap 封装使其在仿真中可见。 表示该 stage 是否正在运行。
      val hartId = wrap(c(Global.HART_ID))
      //  获取 hartId 信号，并用 wrap 封装。
      val fetchId = wrap(c(Fetch.ID))
      //  获取 Fetch ID，并用 wrap 封装。
    }


    val decodes = for (laneId <- 0 until Decode.LANES) yield new Area {
      //  对于每个解码通道（lane），定义一个 Area。
      val c = dpp.ctrl(0).lane(laneId)
      //  获取 DecodePipelinePlugin 中第一个 DecodeStage 对应 lane 的控制信号。
      val fire = wrap(c.up.isFiring)
      //  获取 decode stage 的 isFiring 信号，并用 wrap 封装。
      val spawn = wrap(c.up.transactionSpawn)
      //  获取 spawn 信号，并用 wrap 封装。表示是否产生新的 transaction。
      val hartId = wrap(c(Global.HART_ID))
      //  获取 hartId 信号，并用 wrap 封装。
      val pc = wrap(Global.expendPc(c(Global.PC), 64).asSInt)
      //  获取 PC 信号，并用 wrap 封装。expendPc 将 PC 扩展到 64 位。
      val fetchId = wrap(c(Fetch.ID))
      //  获取 Fetch ID，并用 wrap 封装。
      val decodeId = wrap(c(Decode.DOP_ID))
      //  获取 decode ID，并用 wrap 封装。
    }

    val serializeds = for (laneId <- 0 until Decode.LANES) yield new Area {
      //  对于每个序列化通道（lane），定义一个 Area。
      val decodeAt = host[DecoderPlugin].decodeAt
      //  获取 DecoderPlugin 的 decodeAt。
      val c = dpp.ctrl(decodeAt).lane(laneId)
      //  获取 DecodePipelinePlugin 中 decodeAt 对应 lane 的控制信号。
      host[DecoderPlugin].logic.await()
      //  等待 DecoderPlugin 的 logic 完成。
      val fire = wrap(c.up.transactionSpawn)
      //  获取 fire 信号，并用 wrap 封装。
      val hartId = wrap(c(Global.HART_ID))
      //  获取 hartId 信号，并用 wrap 封装。
      val decodeId = wrap(c(Decode.DOP_ID))
      //  获取 decodeId 信号，并用 wrap 封装。
      val microOpId = wrap(c(Decode.UOP_ID))
      //  获取 microOpId 信号，并用 wrap 封装。
      val microOp = wrap(c(Decode.UOP))
      //  获取 microOp 信号，并用 wrap 封装。
    }

    val dispatches = for (eu <- host.list[ExecuteLaneService]) yield new Area {
      //  对于每个执行单元（EU），定义一个 Area。
      val c = eu.ctrl(0)
      //  获取执行单元的控制信号。
      val fire = wrap(c.down.transactionSpawn)
      //  获取 fire 信号，并用 wrap 封装。
      val hartId = wrap(c(Global.HART_ID))
      //  获取 hartId 信号，并用 wrap 封装。
      val microOpId = wrap(c(Decode.UOP_ID))
      //  获取 microOpId 信号，并用 wrap 封装。
    }


    val executes = for (eu <- host.list[ExecuteLaneService]) yield new Area {
      //  对于每个执行单元（EU），定义一个 Area。
      val c = eu.ctrl(eu.executeAt)
      //  获取执行单元 executeAt 处的控制信号。
      val fire = wrap(c.down.transactionSpawn && c.down(Global.COMMIT))
      //  获取 fire 信号，并用 wrap 封装。
      val hartId = wrap(c(Global.HART_ID))
      //  获取 hartId 信号，并用 wrap 封装。
      val microOpId = wrap(c(Decode.UOP_ID))
      //  获取 microOpId 信号，并用 wrap 封装。
    }

    val withCsr = host.get[CsrAccessPlugin].nonEmpty
    //  判断是否存在 CsrAccessPlugin。
    val csr = withCsr.option(new Area {
      //  如果存在 CsrAccessPlugin，则创建一个 Area。
      val p = host[CsrAccessPlugin].logic
      //  获取 CsrAccessPlugin 的 logic。
      val access = (Flow(new Bundle {
        //  定义 CSR 访问的 Flow 信号。
        val hartId = Global.HART_ID()
        //  hartId。
        val uopId = Decode.UOP_ID()
        //  uopId。
        val address = UInt(12 bits)
        //  CSR 地址。
        val write = Bits(Riscv.XLEN bits)
        //  写入值。
        val read = Bits(Riscv.XLEN bits)
        //  读取值。
        val writeDone = Bool()
        //  写完成标志。
        val readDone = Bool()
        //  读完成标志。
      }))
      access.valid := p.fsm.interface.fire
      //  access.valid 取决于 CsrAccessPlugin 的接口是否 fire。
      access.uopId := p.fsm.interface.uopId
      //  uopId。
      access.hartId := p.fsm.interface.hartId
      //  hartId。
      access.address := U(p.fsm.interface.uop)(Const.csrRange)
      //  CSR 地址。
      access.write := p.fsm.interface.onWriteBits
      //  写入值。
      access.read := p.fsm.interface.csrValue
      //  读取值。
      access.writeDone := p.fsm.interface.write
      //  写完成标志。
      access.readDone := p.fsm.interface.read
      //  读完成标志。
      val port = wrap(access)
      //  用 wrap 封装。
    })

    val rfWrites = new Area {
      //  RegFile 写操作相关的信号集合。
      val ports = host.list[RegFileWriterService].flatMap(_.getRegFileWriters()).map(wrap)
      //  获取所有 RegFileWriterService 提供的 RegFileWriter 端口，并用 wrap 封装。
    }

    val completions = new Area {
      //  完成操作相关的信号集合。
      val ports = host.list[CompletionService].flatMap(cp => cp.getCompletions().map(wrap))
      //  获取所有 CompletionService 提供的 Completion 端口，并用 wrap 封装。
    }

    val commits = new Area {
      //  提交操作相关的信号集合。
      var lanes = host.list[ExecuteLaneService]
      //  获取所有执行单元。
      val trapAt = host[TrapPlugin].trapAt
      //  获取 TrapPlugin 的 trapAt。
      val ctrls = lanes.map(_.execute(trapAt))
      //  获取每个执行单元在 trapAt 处的控制信号。

      case class Commits() extends Bundle{
        //  定义 Commit 信息的 Bundle。
        val pc = Global.PC()
        //  PC。
        val age = Execute.LANE_AGE()
        //  age。
      }
      val ports = for(i <- 0 until ctrls.size) yield new Area{
        //  对于每个执行单元，定义一个 Area。
        val oh = ctrls.map(ctrl => ctrl.down.isFiring && ctrl.down(COMMIT) && ctrl.down(Execute.LANE_AGE) === i)
        //  生成一个 one-hot 编码，表示哪个执行单元在提交。
        val reader = ctrls.reader(oh)
        //  使用 reader 从 one-hot 编码中读取数据。
        val valid = wrap(oh.orR)
        //  valid 信号，表示提交是否有效。
        val pc = wrap(reader(_(Global.PC)))
        //  获取 PC，并用 wrap 封装。
        val uop = wrap(reader(_(Decode.UOP)))
        //  获取 UOP，并用 wrap 封装。

        val uop_rd_addr = wrap(U(reader(_(Decode.UOP))(Const.rdRange)).resize(32))
        // 在 UOP 中切出 rd_addr，并用 wrap 封装。
        val wbp = host[WriteBackPlugin]
        //初始化写回服务的 Plugin
        val reg_data = wrap(wbp.logic.write.port.data)
        // 在wbp中获取写回 reg_data，并用 wrap 封装。
        val reg_valid = wrap(wbp.logic.write.port.valid)
        // 在wbp中获取写回 reg_valid 信号，并用 wrap 封装。
      }
    }

    val reschedules = new Area {
      //  重新调度相关的信号集合。
      val rp = host[ReschedulePlugin]
      //  获取 ReschedulePlugin。
      rp.elaborationLock.await()
      //  等待 elaborationLock 完成。
      val flushes = rp.flushPorts.map(wrap)
      //  获取 flush 端口，并用 wrap 封装。
    }

    val prediction = new Area{
      //  预测相关的信号集合。
      val lp = host[LearnPlugin]
      //  获取 LearnPlugin。
      val learns = lp.logic.ups.map(e => wrap(e.asFlow))
      //  获取 learn 信号，并用 wrap 封装。
    }

    //    val btb = host.get[BtbPlugin]
    //    val btbHit = btb.foreach(btb => new Area {
    //      val fire    = wrap(btb.logic.applyIt.down.isFiring)
    //      val fetchId = wrap(btb.logic.applyIt(Fetch.ID))
    //    })

    val loadExecute = new Area {
      //  Load 执行相关的信号。
      val fire = Bool()
      //  fire 信号。
      val hartId = Global.HART_ID()
      //  hartId 信号。
      val uopId = Decode.UOP_ID()
      //  uopId 信号。
      val size = UInt(2 bits)
      //  load size 信号。
      val address = Global.PHYSICAL_ADDRESS()
      //  load address 信号。
      val data = Bits(Riscv.LSLEN bits)
      //  load data 信号。

      SimPublic(fire, hartId, uopId, size, address, data)
      //  使用 SimPublic 使这些信号在仿真中可见。

      val lcp = host.get[LsuCachelessPlugin] map (p => new Area {
        //  如果存在 LsuCachelessPlugin，则创建一个 Area。
        val c = p.logic.wbCtrl
        //  获取 wbCtrl。
        fire := c.down.isFiring && c(AguPlugin.SEL) && c(AguPlugin.LOAD) && !c(TRAP) && !c(p.logic.onPma.RSP).io
        //  fire 信号的逻辑。表示没有被 trap 且是 load 指令。
        hartId := c(Global.HART_ID)
        //  hartId。
        uopId := c(Decode.UOP_ID)
        //  uopId。
        size := c(AguPlugin.SIZE).resized
        //  size。
        address := c(p.logic.tpk.TRANSLATED)
        //  address。
        data := host.find[IntFormatPlugin](_.lane == p.layer.lane).logic.stages.find(_.ctrlLink == c.ctrlLink).get.wb.payload.resized
        //  data。
        if(p.logic.fpwb.nonEmpty) when(p.logic.fpwb.get.valid){
          //  如果存在 fpwb，并且 valid，则设置 data。
          data := p.logic.fpwb.get.payload.asSInt.resize(widthOf(data)).asBits.resized
          //  data。
        }
      })


      val lp = host.get[LsuPlugin] map (p => new Area {
        //  如果存在 LsuPlugin，则创建一个 Area。
        val c = p.logic.onWb
        //  获取 onWb。
        fire := c.down.isFiring && c(AguPlugin.SEL) && c(AguPlugin.LOAD) && !c(p.logic.LSU_PREFETCH) && !c(TRAP) && !c(p.logic.onPma.IO)
        //  fire 信号的逻辑。表示没有 prefetch，没有被 trap 且是 load 指令。
        hartId := c(Global.HART_ID)
        //  hartId。
        uopId := c(Decode.UOP_ID)
        //  uopId。
        size := c(AguPlugin.SIZE).resized
        //  size。
        address := c(LsuL1.PHYSICAL_ADDRESS)
        //  address。
        data := host.find[IntFormatPlugin](_.lane == p.layer.lane).logic.stages.find(_.ctrlLink == c.ctrlLink).get.wb.payload.resized
        //  data。
        if(p.logic.fpwb.nonEmpty) when(p.logic.fpwb.get.valid){
          //  如果存在 fpwb，并且 valid，则设置 data。
          data := p.logic.fpwb.get.payload.asSInt.resize(widthOf(data)).asBits.resized
          //  data。
        }
      })
    }

    val storeCommit = new Area {
      //  Store 提交相关的信号。
      val fire = Bool()
      //  fire 信号。
      val hartId = Global.HART_ID()
      //  hartId 信号。
      val uopId = Decode.UOP_ID()
      //  uopId 信号。
      val storeId = Decode.STORE_ID()
      //  storeId 信号。
      val size = UInt(2 bits)
      //  store size 信号。
      val address = Global.PHYSICAL_ADDRESS()
      //  store address 信号。
      val data = Bits(Riscv.LSLEN bits)
      //  store data 信号。
      val amo = Bool()
      //  原子操作标志。

      SimPublic(fire, hartId, uopId, storeId, size, address, data, amo)
      //  使用 SimPublic 使这些信号在仿真中可见。

      val lcp = host.get[LsuCachelessPlugin] map (p => new Area {
        //  如果存在 LsuCachelessPlugin，则创建一个 Area。
        val c = p.logic.forkCtrl
        //  获取 forkCtrl。
        val bus = p.logic.bus
        //  获取 bus。
        fire := bus.cmd.fire && bus.cmd.write && !bus.cmd.io
        //  fire 信号的逻辑。
        hartId := c(Global.HART_ID)
        //  hartId。
        uopId := c(Decode.UOP_ID)
        //  uopId。
        size := bus.cmd.size.resized
        //  size。
        address := bus.cmd.address
        //  address。
        data := bus.cmd.data
        //  data。
        storeId := c(Decode.UOP_ID).resized
        //  storeId。
        amo := p.withAmo.mux(bus.cmd.amoEnable, False)
        //  amo。
      })

      val lp = host.get[LsuPlugin] map (p => new Area {
        //  如果存在 LsuPlugin，则创建一个 Area。
        val c = p.logic.onWb
        //  获取 onWb。
        fire := c.storeFire
        //  fire 信号的逻辑。
        hartId := c(Global.HART_ID)
        //  hartId。
        uopId := c(Decode.UOP_ID)
        //  uopId。
        size := c(AguPlugin.SIZE)
        //  size。
        address := c(p.logic.tpk.TRANSLATED)
        //  address。
        data := c(LsuL1.WRITE_DATA)
        //  data。
        storeId := c(Decode.STORE_ID)
        //  storeId。
        amo := False
        //  amo。
      })
    }

    val storeConditional = new Area {
      //  Store 条件操作相关的信号。
      val fire = Bool()
      //  fire 信号。
      val hartId = Global.HART_ID()
      //  hartId 信号。
      val uopId = Decode.UOP_ID()
      //  uopId 信号。
      val miss = Bool()
      //  miss 信号，指示是否 miss。

      SimPublic(fire, hartId, uopId, miss)
      //  使用 SimPublic 使这些信号在仿真中可见。

      val lcp = host.get[LsuCachelessPlugin] map (p => new Area {
        //  如果存在 LsuCachelessPlugin，则创建一个 Area。
        val c = p.logic.wbCtrl
        //  获取 wbCtrl。
        fire := c.down.isFiring && c(AguPlugin.SEL) && (c(AguPlugin.ATOMIC) && !c(AguPlugin.LOAD)) && !c(TRAP)
        //  fire 信号的逻辑。
        hartId := c(Global.HART_ID)
        //  hartId。
        uopId := c(Decode.UOP_ID)
        //  uopId。
        miss := c(p.logic.onJoin.SC_MISS)
        //  miss。
      })
      val lp = host.get[LsuPlugin] map (p => new Area {
        //  如果存在 LsuPlugin，则创建一个 Area。
        val c = p.logic.onWb
        //  获取 onWb。
        fire := c.down.isFiring && c(AguPlugin.SEL) && (c(AguPlugin.ATOMIC) && !c(AguPlugin.LOAD)) && !c(TRAP)
        //  fire 信号的逻辑。
        hartId := c(Global.HART_ID)
        //  hartId。
        uopId := c(Decode.UOP_ID)
        //  uopId。
        miss := c(p.logic.onCtrl.SC_MISS)
        //  miss。
      })
    }

    val storeBroadcast = new Area {
      //  Store 广播相关的信号。
      val fire = Bool()
      //  fire 信号。
      val hartId = Global.HART_ID()
      //  hartId 信号。
      val storeId = Decode.STORE_ID()
      //  storeId 信号。

      SimPublic(fire, hartId, storeId)
      //  使用 SimPublic 使这些信号在仿真中可见。

      val lcp = host.get[LsuCachelessPlugin] map (p => new Area {
        //  如果存在 LsuCachelessPlugin，则创建一个 Area。
        fire    := storeCommit.fire
        //  fire 信号。
        hartId  := storeCommit.hartId
        //  hartId。
        storeId := storeCommit.storeId
        //  storeId。
      })
      val lp = host.get[LsuPlugin] map (p => new Area {
        //  如果存在 LsuPlugin，则创建一个 Area。
        val c = p.logic.onWb
        //  获取 onWb。
        fire := c.storeBroadcast
        //  fire 信号。
        hartId := c(Global.HART_ID)
        //  hartId。
        storeId := c(Decode.STORE_ID)
        //  storeId。
      })
    }

    val wfi = wrap(host[TrapPlugin].logic.harts.map(_.trap.fsm.wfi).asBits)
    //  获取 WFI 信号，并用 wrap 封装。

    val perf = new Area{
      //  性能计数器相关的信号集合。
      val dispatch = host[DispatchPlugin]
      //  获取 DispatchPlugin。
      val executeFreezed = wrap(host[ExecutePipelinePlugin].isFreezed())
      //  获取 executeFreezed 信号，并用 wrap 封装。
      val dispatchHazards = wrap(dispatch.logic.candidates.map(c => c.ctx.valid && !c.fire).orR)
      //  获取 dispatchHazards 信号，并用 wrap 封装。
      val candidatesCount = wrap(CountOne(dispatch.logic.candidates.map(_.ctx.valid)))
      //  获取 candidatesCount 信号，并用 wrap 封装。
      val dispatchFeedCount = CountOne(dispatch.logic.feeds.map(_.isValid))
      //  获取 dispatchFeedCount 信号。

      val executeFreezedCounter = wrap(Counter(1l << 60l, executeFreezed).value)
      //  计算 executeFreezed 的计数。
      val dispatchHazardsCounter = wrap(Counter(1l << 60l, dispatchHazards).value)
      //  计算 dispatchHazards 的计数。
      val candidatesCountCounters = (0 to dispatch.logic.candidates.size).map(id => wrap(Counter(1l << 60l, candidatesCount === id).value))
      //  计算 candidatesCount 的计数。
      val dispatchFeedCounters = (0 to dispatch.logic.feeds.size).map(id => wrap(Counter(1l << 60l, dispatchFeedCount === id).value))
      //  计算 dispatchFeedCount 的计数。
    }

    val trap = new Area {
      //  Trap 相关的信号集合。
      val ports = for(hartId <- 0 until HART_COUNT) yield new Area{
        //  对于每个 hart，定义一个 Area。
        val priv = host[TrapPlugin].logic.harts(hartId).trap
        //  获取 TrapPlugin 中每个 hart 的 trap 信号。
        val valid = wrap(priv.whitebox.trap)
        //  获取 trap valid 信号，并用 wrap 封装。
        val interrupt = wrap(priv.whitebox.interrupt)
        //  获取 interrupt 信号，并用 wrap 封装。
        val cause = wrap(priv.whitebox.code)
        //  获取 trap cause 信号，并用 wrap 封装。
      }
    }

    def self = this
    //  定义 self。
    abstract class Proxies {
      //  定义一个抽象类 Proxies，用于封装所有信号，方便在仿真中使用。
      val fetch = new FetchProxy()
      //  Fetch 相关的 Proxy。
      val decodes = self.decodes.indices.map(new DecodeProxy(_)).toArray
      //  Decode 相关的 Proxy。
      val serializeds = self.serializeds.indices.map(new SerializedProxy(_)).toArray
      //  Serialized 相关的 Proxy。
      val dispatches = self.dispatches.indices.map(new DispatchProxy(_)).toArray
      //  Dispatch 相关的 Proxy。
      val executes = self.executes.indices.map(new ExecuteProxy(_)).toArray
      //  Execute 相关的 Proxy。
      val csr = self.csr.map(_ => new CsrProxy())
      //  CSR 相关的 Proxy。
      val rfWrites = self.rfWrites.ports.map(new RfWriteProxy(_)).toArray
      //  RegFileWrite 相关的 Proxy。
      val completions = self.completions.ports.map(new CompletionProxy(_)).toArray
      //  Completions 相关的 Proxy。
      val flushes = self.reschedules.flushes.map(new FlushProxy(_)).toArray
      //  Flush 相关的 Proxy。
      val loadExecute = new LoadExecuteProxy()
      //  LoadExecute 相关的 Proxy。
      val storeCommit = new StoreCommitProxy()
      //  StoreCommit 相关的 Proxy。
      val storeConditional = new StoreConditionalProxy()
      //  StoreConditional 相关的 Proxy。
      val storeBroadcast = new StoreBroadcastProxy()
      //  StoreBroadcast 相关的 Proxy。
      val learns = self.prediction.learns.map(learn => new LearnProxy(learn)).toArray
      //  Learn 相关的 Proxy。
      val perf = new PerfProxy()
      //  Perf 相关的 Proxy。
      val trap = self.trap.ports.indices.map(new TrapProxy(_)).toArray
      //  Trap 相关的 Proxy。
      val interrupts = new InterruptsProxy()
      //  Interrupts 相关的 Proxy。
      val wfi = self.wfi.simProxy()
      //  WFI 相关的 Proxy。

      def interrupt(hartId : Int, intId : Int, value : Boolean)
      //  抽象方法，用于设置中断。

      class InterruptChecker(hartId : Int, pin: Bool, id: Int) {
        //  用于检查中断状态的辅助类。
        val proxy = pin.simProxy()
        //  中断信号的仿真代理。
        var last = proxy.toBoolean
        //  上次中断状态。

        def sync(): Unit = {
          //  同步中断状态。
          interrupt(hartId, id, last)
        }

        def check(): Unit = {
          //  检查中断状态是否改变。
          val value = proxy.toBoolean
          //  当前中断状态。
          if (value != last) {
            interrupt(hartId, id, value)
            //  如果状态改变，则更新中断状态。
            last = value
            //  更新上次状态。
          }
        }
      }

      class InterruptsProxy {
        //  中断相关的 Proxy。
        val priv = host[PrivilegedPlugin]
        //  获取 PrivilegedPlugin。
        val checkers = ArrayBuffer[InterruptChecker]()
        //  中断检查器的列表。
        for ((hart, hartId) <- priv.logic.harts.zipWithIndex) {
          //  为每个 hart 创建中断检查器。
          checkers += new InterruptChecker(hartId, hart.int.m.timer,  7)
          //  添加 M-mode timer 中断检查器。
          checkers += new InterruptChecker(hartId, hart.int.m.software,  3)
          //  添加 M-mode 软件中断检查器。
          checkers += new InterruptChecker(hartId, hart.int.m.external, 11)
          //  添加 M-mode 外部中断检查器。
          if (priv.p.withSupervisor) {
            //  如果存在 Supervisor 模式。
            checkers += new InterruptChecker(hartId, hart.int.s.external, 9)
            //  添加 S-mode 外部中断检查器。
          }
        }
        def check(): Unit = {
          //  检查所有中断状态。
          checkers.foreach(_.check())
        }

        def sync(): Unit = {
          //  同步所有中断状态。
          checkers.foreach(_.sync())
        }
      }
    }


    class FetchProxy {
      //  Fetch 相关的 Proxy 类。
      val fire = fetch.fire.simProxy()
      //  fetch.fire 的仿真代理。
      val hartd = fetch.hartId.simProxy()
      //  fetch.hartId 的仿真代理。
      val id = fetch.fetchId.simProxy()
      //  fetch.fetchId 的仿真代理。
    }

    class DecodeProxy(laneId: Int) {
      //  Decode 相关的 Proxy 类，针对每个 lane。
      val self = decodes(laneId)
      //  获取对应 lane 的 Decode area。
      val spawn = self.spawn.simProxy()
      //  self.spawn 的仿真代理。
      val fire = self.fire.simProxy()
      //  self.fire 的仿真代理。
      val hartId = self.hartId.simProxy()
      //  self.hartId 的仿真代理。
      val pc = self.pc.simProxy()
      //  self.pc 的仿真代理。
      val fetchId = self.fetchId.simProxy()
      //  self.fetchId 的仿真代理。
      val decodeId = self.decodeId.simProxy()
      //  self.decodeId 的仿真代理。
    }

    class SerializedProxy(laneId: Int) {
      //  Serialized 相关的 Proxy 类，针对每个 lane。
      val self = serializeds(laneId)
      //  获取对应 lane 的 Serialized area。
      val fire = self.fire.simProxy()
      //  self.fire 的仿真代理。
      val hartId = self.hartId.simProxy()
      //  self.hartId 的仿真代理。
      val decodeId = self.decodeId.simProxy()
      //  self.decodeId 的仿真代理。
      val microOpId = self.microOpId.simProxy()
      //  self.microOpId 的仿真代理。
      val microOp = self.microOp.simProxy()
      //  self.microOp 的仿真代理。
    }

    class DispatchProxy(laneId: Int) {
      //  Dispatch 相关的 Proxy 类，针对每个 lane。
      val self = dispatches(laneId)
      //  获取对应 lane 的 Dispatch area。
      val fire = self.fire.simProxy()
      //  self.fire 的仿真代理。
      val hartId = self.hartId.simProxy()
      //  self.hartId 的仿真代理。
      val microOpId = self.microOpId.simProxy()
      //  self.microOpId 的仿真代理。
    }

    class ExecuteProxy(laneId: Int) {
      //  Execute 相关的 Proxy 类，针对每个 lane。
      val self = executes(laneId)
      //  获取对应 lane 的 Execute area。
      val fire = self.fire.simProxy()
      //  self.fire 的仿真代理。
      val hartId = self.hartId.simProxy()
      //  self.hartId 的仿真代理。
      val microOpId = self.microOpId.simProxy()
      //  self.microOpId 的仿真代理。
    }


    class CsrProxy{
      //  CSR 相关的 Proxy 类。
      val csr = self.csr.get
      //  获取 csr。
      val valid = csr.port.valid.simProxy()
      //  csr.port.valid 的仿真代理。
      val hartId = csr.port.hartId.simProxy()
      //  csr.port.hartId 的仿真代理。
      val uopId = csr.port.uopId.simProxy()
      //  csr.port.uopId 的仿真代理。
      val address = csr.port.address.simProxy()
      //  csr.port.address 的仿真代理。
      val write = csr.port.write.simProxy()
      //  csr.port.write 的仿真代理。
      val read = csr.port.read.simProxy()
      //  csr.port.read 的仿真代理。
      val writeDone = csr.port.writeDone.simProxy()
      //  csr.port.writeDone 的仿真代理。
      val readDone = csr.port.readDone.simProxy()
      //  csr.port.readDone 的仿真代理。
    }

    class RfWriteProxy(val port : Flow[RegFileWriter]) {
      //  RfWrite 相关的 Proxy 类。
      val valid = port.valid.simProxy()
      //  port.valid 的仿真代理。
      val data = port.data.simProxy()

      val hartId = port.hartId.simProxy()
      //  port.hartId 的仿真代理。
      val uopId = port.uopId.simProxy()
      //  port.uopId 的仿真代理。
    }

    class CompletionProxy(port: Flow[CompletionPayload]) {
      //  Completion 相关的 Proxy 类。
      val valid = port.valid.simProxy()
      //  port.valid 的仿真代理。
      val hartId = port.hartId.simProxy()
      //  port.hartId 的仿真代理。
      val uopId = port.uopId.simProxy()
      //  port.uopId 的仿真代理。
      val trap = port.trap.simProxy()
      //  port.trap 的仿真代理。
      val commit = port.commit.simProxy()
      //  port.commit 的仿真代理。
    }

    class commitsProxy(port:)
    class FlushProxy(port: Flow[FlushCmd]) {
      //  Flush 相关的 Proxy 类。
      val withUopId = port.withUopId
      //  withUopId 信号。
      val valid = port.valid.simProxy()
      //  port.valid 的仿真代理。
      val hartId = port.hartId.simProxy()
      //  port.hartId 的仿真代理。
      val uopId = port.withUopId generate port.uopId.simProxy()
      //  port.uopId 的仿真代理，如果 withUopId 为 true。
      val laneAge = port.laneAge.simProxy()
      //  port.laneAge 的仿真代理。
      val self = port.self.simProxy()
      //  port.self 的仿真代理。
    }

    class LoadExecuteProxy {
      //  LoadExecute 相关的 Proxy 类。
      val fire = loadExecute.fire.simProxy()
      //  loadExecute.fire 的仿真代理。
      val hartId = loadExecute.hartId.simProxy()
      //  loadExecute.hartId 的仿真代理。
      val uopId = loadExecute.uopId.simProxy()
      //  loadExecute.uopId 的仿真代理。
      val size = loadExecute.size.simProxy()
      //  loadExecute.size 的仿真代理。
      val address = loadExecute.address.simProxy()
      //  loadExecute.address 的仿真代理。
      val data = loadExecute.data.simProxy()
      //  loadExecute.data 的仿真代理。
    }

    class StoreCommitProxy {
      //  StoreCommit 相关的 Proxy 类。
      val fire = storeCommit.fire.simProxy()
      //  storeCommit.fire 的仿真代理。
      val hartId = storeCommit.hartId.simProxy()
      //  storeCommit.hartId 的仿真代理。
      val uopId = storeCommit.uopId.simProxy()
      //  storeCommit.uopId 的仿真代理。
      val storeId = storeCommit.storeId.simProxy()
      //  storeCommit.storeId 的仿真代理。
      val size = storeCommit.size.simProxy()
      //  storeCommit.size 的仿真代理。
      val address = storeCommit.address.simProxy()
      //  storeCommit.address 的仿真代理。
      val data = storeCommit.data.simProxy()
      //  storeCommit.data 的仿真代理。
      val amo = storeCommit.amo.simProxy()
      //  storeCommit.amo 的仿真代理。
    }

    class StoreConditionalProxy {
      //  StoreConditional 相关的 Proxy 类。
      val fire = storeConditional.fire.simProxy()
      //  storeConditional.fire 的仿真代理。
      val hartId = storeConditional.hartId.simProxy()
      //  storeConditional.hartId 的仿真代理。
      val uopId = storeConditional.uopId.simProxy()
      //  storeConditional.uopId 的仿真代理。
      val miss = storeConditional.miss.simProxy()
      //  storeConditional.miss 的仿真代理。
    }

    class StoreBroadcastProxy {
      //  StoreBroadcast 相关的 Proxy 类。
      val fire = storeBroadcast.fire.simProxy()
      //  storeBroadcast.fire 的仿真代理。
      val hartId = storeBroadcast.hartId.simProxy()
      //  storeBroadcast.hartId 的仿真代理。
      val storeId = storeBroadcast.storeId.simProxy()
      //  storeBroadcast.storeId 的仿真代理。
    }

    class LearnProxy(port: Flow[LearnCmd]) {
      //  Learn 相关的 Proxy 类。
      val valid = port.valid.simProxy()
      //  port.valid 的仿真代理。
      val pcOnLastSlice = port.pcOnLastSlice.simProxy()
      //  port.pcOnLastSlice 的仿真代理。
      val pcTarget = port.pcTarget.simProxy()
      //  port.pcTarget 的仿真代理。
      val taken = port.taken.simProxy()
      //  port.taken 的仿真代理。
      val isBranch = port.isBranch.simProxy()
      //  port.isBranch 的仿真代理。
      val wasWrong = port.wasWrong.simProxy()
      //  port.wasWrong 的仿真代理。
      val history = port.history.simProxy()
      //  port.history 的仿真代理。
      val uopId = port.uopId.simProxy()
      //  port.uopId 的仿真代理。
      val hartId = port.hartId.simProxy()
      //  port.hartId 的仿真代理。
    }

    class PerfProxy() {
      //  性能计数器相关的 Proxy 类。
      val candidatesMax = perf.dispatch.logic.candidates.size
      //  dispatch.logic.candidates 的大小。
      val executeFreezed = perf.executeFreezed.simProxy()
      //  perf.executeFreezed 的仿真代理。
      val dispatchHazards = perf.dispatchHazards.simProxy()
      //  perf.dispatchHazards 的仿真代理。
      val candidatesCount = perf.candidatesCount.simProxy()
      //  perf.candidatesCount 的仿真代理。
    }

    class TrapProxy(val hartId: Int) {
      //  Trap 相关的 Proxy 类，针对每个 hart。
      val self = trap.ports(hartId)
      //  获取对应 hart 的 trap area。
      val fire = self.valid.simProxy()
      //  self.valid 的仿真代理。
      val interrupt = self.interrupt.simProxy()
      //  self.interrupt 的仿真代理。
      val cause = self.cause.simProxy()
      //  self.cause 的仿真代理。
    }

    buildBefore.release()
    //  释放 buildBefore 的 lock。
  }
}