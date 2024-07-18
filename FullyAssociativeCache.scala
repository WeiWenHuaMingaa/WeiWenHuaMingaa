import chisel3._
import chisel3.util._

class FullyAssociativeCache(addrWidth: Int) extends Module {
  val io = IO(new Bundle {
    val addr = Input(UInt(addrWidth.W))
    val dataIn = Input(UInt(128.W))  // 16 bytes
    val dataOut = Output(UInt(128.W))
    val read = Input(Bool())
    val write = Input(Bool())
    val hit = Output(Bool())
  })

  // Cache storage
  val cache = Reg(Vec(16, new Bundle {
    val valid = Bool()
    val tag = UInt((addrWidth - 4).W)  // Total 16 lines, so 4 bits for index
    val data = Vec(16, UInt(8.W))  // 16 bytes per line
    val timestamp = UInt(32.W)
  }))

  // Internal signals
  val tag = io.addr(addrWidth - 1, 4)  // Tag part of address
  val offset = io.addr(3, 0)  // Offset part of address

  // Read and write operations
  val hit = Wire(Bool())
  val hitIndex = Wire(UInt(4.W))
  hit := false.B
  hitIndex := 0.U

  // Hit and miss detection
  for (i <- 0 until 16) {
    when(cache(i).valid && cache(i).tag === tag) {
      hit := true.B
      hitIndex := i.U
    }
  }
  io.hit := hit

  // FIFO replacement policy
  val oldestIndex = Wire(UInt(4.W))
  val oldestTimestamp = RegInit(UInt(32.W), 0.U)
  oldestIndex := 0.U
  oldestTimestamp := ~0.U(32.W)

  for (i <- 0 until 16) {
    when(cache(i).valid && cache(i).timestamp < oldestTimestamp) {
      oldestTimestamp := cache(i).timestamp
      oldestIndex := i.U
    }
  }

  // Data handling for reads and writes
  when(io.write) {
    when(hit) {
      // 命中，更新数据
      for (i <- 0 until 16) {
        when(i.U === offset) {
          cache(hitIndex).data(i) := io.dataIn(8 * (i + 1) - 1, 8 * i)
        }
      }
      cache(hitIndex).timestamp := 0.U
    }.otherwise {
      // 未命中，使用FIFO替换策略更新数据
      cache(oldestIndex).valid := true.B
      cache(oldestIndex).tag := tag
      for (i <- 0 until 16) {
        cache(oldestIndex).data(i) := io.dataIn(8 * (i + 1) - 1, 8 * i)
      }
      cache(oldestIndex).timestamp := 0.U
    }
  }

  when(io.read) {
    when(hit) {
      // 命中，读取数据
      io.dataOut := cache(hitIndex).data.asUInt
      cache(hitIndex).timestamp := 0.U
    }.otherwise {
      // 未命中，数据输出为0
      io.dataOut := 0.U
    }
  }

  // 更新时间戳
  for (i <- 0 until 16) {
    when(cache(i).valid) {
      cache(i).timestamp := cache(i).timestamp + 1.U
    }
  }
}

object FullyAssociativeCache extends App {
  chisel3.Driver.execute(args, () => new FullyAssociativeCache(32))
}
