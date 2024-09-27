import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.Stack
import scala.collection.mutable.HashMap.Node

@main def hello(): Unit = {

  val st = Stack[Int]()
  val pq = mutable.PriorityQueue[Int]()
  val temp = List()
  println("Hello World")
  val test = new JimsHashMap[String, String]()

  test.add("0-t", "testValue")
  test.add("0-t", "0 testValue=0")

  (1 to 24).foreach(x => {
      test.add(s"$x-t", s"$x testValue=$x")
  })

  (0 to 24).foreach(x => {
    val r = test.get(s"$x-t")

    if (r != None) {
      val res = (r.get.equals(s"$x testValue=$x"))
      println(s"$x = ${test.get(s"$x-t").get} = $res")
    } else {
      println(s"Missing: $x")
    }
  })

  test.printMe()

  println("% 2 removed")

  (0 to 24).foreach(x => {
    if (x % 2 == 0) {
      test.remove(s"$x-t")
    }
  })

  test.printMe()

  (0 to 24).foreach(x => {
    val r = test.get(s"$x-t")

    if (r != None) {
      val res = (r.get.equals(s"$x testValue=$x"))
      println(s"$x = ${test.get(s"$x-t").get} = $res")
    } else {
      println(s"Missing: $x")
    }
  })

}

final case class Node[K, V](key: K, var value: V, hash: Int, var next: Node[K, V]) {

  /**
   * This hashMap implementation uses Separate chaining (open hashing).
   *
   * Each Node has the ability to link to another node. This is used when there is a collision on the table index.
   * This recursive function loops through the nodes in that index and finds the one with the right hash and key
   */
  @tailrec
  def findNode(k: K, h: Int): Node[K, V] = {
    if(h == hash && k == key) this
    else if ((next eq null) || (hash > h)) null
    else next.findNode(k, h)
  }
}

class JimsHashMap[K, V](initialSize: Int = 16) {
  var table = new Array[Node[K, V]](initialSize)
  var currentSize = 0

  def printMe() = {
    table.foreach(x => {
      if (x != null) {
        println(s"h${x.hash} -> K= ${x.key} , V= ${x.value}")
        var next = if (x.next != null) x.next else null
        while(next != null) {
          println(s"h${next.hash} -> K= ${next.key} , V= ${next.value}")
          next = next.next
        }
      }
    })
  }

  def add(key: K, value: V): JimsHashMap[K, V] = {
    if (currentSize + 1 > table.length) growTable()
    val index = indexFor(key)
//    println(s"$index -> $key")

    table(index) match {
      case null => {
        currentSize += 1
        table(index) = Node(key, value, index, null)
      }
      case old => {
        var prev: Node[K, V] = null
        var n = old
        // Resolve collissions with Separate chaining (open hashing)
        while((n ne null) && n.hash <= index) {
          if(n.hash == index && key == n.key) {
            val old = n.value
            n.value = value
            return null
          }

          prev = n
          n = n.next
        }
        if(prev eq null) table(index) = new Node(key, value, index, prev.next)
        else prev.next = new Node(key, value, index, prev.next)
      }
    }

    this
  }

  def remove(key: K): Node[K, V] = {
    val hash = indexFor(key)
    table(hash) match {
      case null => null
      case nd if nd.hash == hash && nd.key == key => {
        table(hash) = nd.next
        currentSize -= 1
        nd
      }
      case nd => {
        var prev = nd
        var next = nd.next
        while ((next ne null) && next.hash <= hash) {
          if (next.hash == hash && next.key == key) {
            prev.next = next.next
            currentSize -= 1
            return next
          }

          prev = next
          next = next.next
        }
        null
      }
    }
  }

  def get(key: K): Option[V] = {
    val index = indexFor(key)
    table(index) match {
      case null => None
      case nd => {
        val node = nd.findNode(key, index)
        if (node != null) {
          Some(node.value)
        } else {
          None
        }
      }
    }
  }

  private def indexFor(key: K): Int = {
    key.hashCode() & table.size - 1
  }

  private def growTable() = {
    val newTable = new Array[Node[K, V]](table.length * 2)
    table.indices.foreach(x => {
      println(x)

      table(x) match {
        case null =>
        case nd => {
          val i = indexFor(nd.key)
          newTable(i) = Node(nd.key, nd.value, i, null)
        }
      }
    })

    newTable.mkString(",")
    table = newTable.clone()
  }
}


