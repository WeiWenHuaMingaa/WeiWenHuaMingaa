import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class FullyAssociativeCacheTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "FullyAssociativeCache"

  // 实现基本读/写测试
  it should "handle basic read and write operations" in {
    test(new FullyAssociativeCache(addrWidth = 32)) { c =>
      // 写入数据
      c.io.write.poke(true.B)
      c.io.read.poke(false.B)
      c.io.addr.poke("h00000000".U)
      c.io.dataIn.poke("h_dead_beef_dead_beef_dead_beef_dead_beef".U)
      c.clock.step(1)

      // 读取数据
      c.io.write.poke(false.B)
      c.io.read.poke(true.B)
      c.io.addr.poke("h00000000".U)
      c.clock.step(1)
      c.io.dataOut.expect("h_dead_beef_dead_beef_dead_beef_dead_beef".U)
    }
  }

  // 实现FIFO替换策略测试
  it should "correctly implement FIFO replacement policy" in {
    test(new FullyAssociativeCache(addrWidth = 32)) { c =>
      // 填满缓存
      for (i <- 0 until 16) {
        c.io.write.poke(true.B)
        c.io.read.poke(false.B)
        c.io.addr.poke(i.U << 4) // 每个地址偏移16字节
        c.io.dataIn.poke((i * 0x11111111).U)
        c.clock.step(1)
      }

      // 写入第17条数据，应触发FIFO替换
      c.io.write.poke(true.B)
      c.io.read.poke(false.B)
      c.io.addr.poke("h00000000".U)
      c.io.dataIn.poke("h_ffffffff_ffffffff_ffffffff_ffffffff".U)
      c.clock.step(1)

      // 检查第一个条目是否已被替换
      c.io.write.poke(false.B)
      c.io.read.poke(true.B)
      c.io.addr.poke("h00000000".U)
      c.clock.step(1)
      c.io.dataOut.expect("h_ffffffff_ffffffff_ffffffff_ffffffff".U)

      // 检查第二条目是否仍然存在
      c.io.write.poke(false.B)
      c.io.read.poke(true.B)
      c.io.addr.poke("h00000010".U) // 16 bytes offset for next entry
      c.clock.step(1)
      c.io.dataOut.expect("h_11111111_11111111_11111111_11111111".U)
    }
  }

  // 实现随机访问测试
  it should "handle random access patterns" in {
    test(new FullyAssociativeCache(addrWidth = 32)) { c =>
      // 写入一些数据到缓存中
      c.io.write.poke(true.B)
      c.io.read.poke(false.B)
      for (i <- 0 until 8) {
        val addr = (scala.util.Random.nextInt(16) << 4).U // 生成随机地址偏移
        val data = scala.util.Random.nextInt(256).U           // 生成随机数据
        c.io.addr.poke(addr)
        c.io.dataIn.poke(data)
        c.clock.step(1)
      }

      // 随机读取数据
      c.io.write.poke(false.B)
      c.io.read.poke(true.B)
      for (i <- 0 until 8) {
        val addr = (scala.util.Random.nextInt(16) << 4).U // 生成随机地址偏移
        c.io.addr.poke(addr)
        c.clock.step(1)
        c.io.dataOut.expect(c.cache(i).data.asUInt) // 验证读取的数据是否正确
      }
    }
  }
}

object FullyAssociativeCacheTest extends App {
  chisel3.Driver.execute(args, () => new FullyAssociativeCache(addrWidth = 32))
}
