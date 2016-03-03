package com.pokarim.pprint.tests

import org.specs2.mutable._ 
import scala.Console.withOut
import java.io.Writer
import java.io.ByteArrayOutputStream
import com.pokarim.pprint._
import com.pokarim.pprint.ColorUtil//.getLen

class PPrintSpec extends Specification {

  def getOutput(f: => Unit):String = {
	val out = new ByteArrayOutputStream();
	withOut(out){f}
	out.toString()
  }

  def getOutputWithoutColor(f: => Unit):String = {
	val out = new ByteArrayOutputStream();
	withOut(out){enableColor.withValue(false){f}}
	out.toString()
  }

  "The 'Hello world' string" should {
	"ColoUtil.getLen" in {
	  import ColorUtil._
	  List("List","Hoge(").forall(
		(s) =>getLen(s) == getLen(withColor(10,s))) === 
		true
	}
	"getOutput" in {
	  getOutput(println("xx")) === "xx\n"
	  getOutput(print("yy")) === "yy"
	  getOutput(print("z\nz")) === "z\nz"
	}

	val ListOfMap01 = List(Map(1->2), Map(1->2), Map(1->2),
						   Map(1->2), Map(1->2), Map(1->2),Map(1->2))

	val ListOfMap01Str = """List(Map(1 -> 2),
     Map(1 -> 2),
     Map(1 -> 2),
     Map(1 -> 2),
     Map(1 -> 2),
     Map(1 -> 2),
     Map(1 -> 2))"""

    "pprn" in {
	  getOutputWithoutColor(pprn(List(1,2,3))) ===
		"List(1, 2, 3)\n"
	  getOutputWithoutColor(pprn(Vector(1,2,3))) ===
		"Vector(1, 2, 3)\n"
	  getOutputWithoutColor(pprn(ListOfMap01)) === ListOfMap01Str ++ "\n"
	}

    "case class" in {
      case class Hoge(a:Int,b:Int,c:String)
      case class Hog1(a:Int)
      case class Hog2(a:Int,b:Int)
      implicit val f3 = DOC.asProduct3(Hoge.unapply(_:Hoge).get)
      implicit val f2 = DOC.asProduct2(Hog2.unapply(_:Hog2).get)
      implicit val f1 = DOC.asProduct1(Hog1.unapply(_:Hog1).get )
      lineWidth.withValue(3){
        getOutputWithoutColor(pprn(Hoge(1,2,"a"))) ===
          "Hoge(1,\n     2,\n     \"a\")\n"
        getOutputWithoutColor(pprn(Hog2(1,2))) === "Hog2(1,\n     2)\n"
        getOutputWithoutColor(pprn(Hog1(1))) === "Hog1(1)\n"
      }
    }
    "pprn" in {
      getOutputWithoutColor(pprn("a")) === "\"a\"\n"
	  getOutputWithoutColor(pprn(List(1,2,3))) ===
		"List(1, 2, 3)\n"
	  getOutputWithoutColor(pprn(Vector(1,2,3))) ===
		"Vector(1, 2, 3)\n"
	  getOutputWithoutColor(pprn(ListOfMap01)) === ListOfMap01Str ++ "\n"
	}

    "toPrettyString" in {
	  enableColor.withValue(false){toPrettyString(List(1, 2, 3))} === 
		"List(1, 2, 3)"
	  enableColor.withValue(false){toPrettyString(ListOfMap01)} === 
		ListOfMap01Str
	}
  }
}
