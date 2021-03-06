package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
  trait TestTrees {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }

  test("makeCodeTree testing with weight and chars") {
    val codeTree = makeCodeTree(Leaf('a',2), Leaf('b',3))
    assert(codeTree.chars == List('a', 'b'))
    assert(codeTree.weight == 5)
  }
  
  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }
  
  test("times function with one element") {
    assert(times(List('a')) == List(('a', 1)))
    
    val huffList = times(List('a', 'b'))
    assert(huffList == List(('a', 1), ('b', 1)) || huffList == List(('b', 1), ('a', 1)))
  }

  test("times function with unique elements") {
    val huffList = times(List('a', 'b'))
    assert(huffList == List(('a', 1), ('b', 1)) || huffList == List(('b', 1), ('a', 1)))
  }
  
  test("times function with repeated elements") {
    val huffList = times(List('a', 'b', 'b'))
    assert(huffList == List(('a', 1), ('b', 2)) || huffList == List(('b', 2), ('a', 1)))
  }
  
  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }
}
