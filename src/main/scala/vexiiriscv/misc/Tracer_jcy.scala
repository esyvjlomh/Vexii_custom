//package vexiiriscv.misc
//
//import spinal.core._
//import spinal.lib._
//import spinal.lib.misc.pipeline._
//import spinal.lib.misc.plugin.FiberPlugin
//import vexiiriscv.Global._
//import vexiiriscv.{VexiiRiscv, riscv}
//import vexiiriscv.decode.Decode
//import vexiiriscv.decode.Decode._
//import vexiiriscv.execute.{LaneLayer, WriteBackPlugin}
//import vexiiriscv.riscv.Const.rdRange
//import vexiiriscv.riscv.IntRegFile
//import vexiiriscv.test.WhiteboxerPlugin
//
//// 定义了一个简单的数据结构，用于传递跟踪数据
//case class Tracer_simple_data() extends Bundle with IMasterSlave {
//  // 寄存器写入有效标志
//  val reg_write_valid = Bool()
//  // 写入的寄存器地址
//  val reg_write_addr  = UInt(32 bits)
//  // 写入的数据
//  val reg_write_data  = Bits(32 bits)
//  // 写入时的程序计数器（PC）
//  val WB_pc           = UInt(32 bits)
//  // 读取的指令
//  val WB_instruction  = Bits(32 bits)
//
//  // 定义作为主设备时的信号输出
//  override def asMaster(): Unit = {
//    // 将信号输出到总线
//    out(reg_write_valid, reg_write_addr, reg_write_data, WB_pc, WB_instruction)
//  }
//}
//
//// 定义简单的跟踪总线结构
//case class Tracer_simple_bus(rv32e: Boolean = false) extends Bundle with IMasterSlave {
//  // 使用流结构来传递跟踪数据
//  val data = Stream(Tracer_simple_data())
//
//  // 定义作为主设备时的信号输出
//  override def asMaster(): Unit = {
//    // 将数据流定义为主设备类型
//    master(data)
//  }
//}
//
//// 定义跟踪插件
//class Tracer_Plugin_zsh(val layer : LaneLayer) extends FiberPlugin {
//  // 声明一个总线变量，用于数据传输
//  var Tracer_bus: Tracer_simple_bus = null
//
//  // 标识是否为 RV32E 架构
//  val rv32e: Boolean = false
//
//  val logic = during build new Area {
//    // 设置阶段，初始化插件
//    val wbp = host.find[WriteBackPlugin](p => p.rf == IntRegFile && p.lane == layer.lane)
//    val earlyLock = retains(layer.lane.uopLock, wbp.elaborationLock)
//    val lateLock = retains(layer.lane.pipelineLock)
//    awaitBuild()
//
//    val wb = wbp.createPort(at = 0)
//
//    val SEL = Payload(Bool())
//    layer.lane.setDecodingDefault(SEL, False)
//
//    earlyLock.release()
//    Tracer_bus = master(Tracer_simple_bus(rv32e).setName("Tracer_bus"))
//
//    // 构建阶段，定义如何将输出连接到流中
//    override def build(pipeline:VexiiRiscv): Unit = {
//
//      // 根据 RV32E 决定寄存器的范围裁剪
//      def clipRange(that: Range) = if (rv32e) that.tail else that
//
//      // 根据是否为 RV32E 决定寄存器数量，RV32E 有16个寄存器，RV32 有32个
//      val Registers_num = if (rv32e) 16 else 32
//
//      // 将有效信号与调度器的触发状态连接
//      Tracer_bus.data.valid := wb.fire
//      // 设置寄存器写入有效信号
//      Tracer_bus.data.payload.reg_write_valid := wb.valid
//      // 通过指令的 rd 字段得到写入的寄存器地址，并调整大小
//      Tracer_bus.data.payload.reg_write_addr := Decode.INSTRUCTION(riscv.Const.rdRange)
//      // 从输出中获取写入的数据
//      Tracer_bus.data.payload.reg_write_data := wb.payload
//      // 将当前 PC 输出到跟踪总线
//      Tracer_bus.data.payload.WB_pc := PC
//      // 将当前指令输出到跟踪总线
//      Tracer_bus.data.payload.WB_instruction := Reg(UInt(Decode.INSTRUCTION bits))
//
//    }
//
//  }
//}
