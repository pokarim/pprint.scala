/**
 * Copyright 2014 Mikio Hokari
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 *  * you may not use this file except in compliance with the License.
 *  * You may obtain a copy of the License at
 *  *
 *  *     http://www.apache.org/licenses/LICENSE-2.0
 *
 *  * Unless required by applicable law or agreed to in writing, software
 *  * distributed under the License is distributed on an "AS IS" BASIS,
 *  * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  * See the License for the specific language governing permissions and
 *  * limitations under the License.
 *  */

package com.pokarim.pprint

object Docs {
  import scala.language.implicitConversions
  trait Doc
  class StringWrapper(s : String){
	def Text(d: =>Doc): Doc = new Text(s,d)
  }
  class IntWrapper(i : Int){
	def Line(d: =>Doc): Doc = new Line(i,d)
  }
  class Text(val s:String, _d: =>Doc) extends Doc{
	def d = _d
  }
  object Text {
	def unapply(t:Text) = Some((t.s,t.d))
  }

  class Line(val i:Int, _d: =>Doc) extends Doc{
	def d = _d
  }
  object Line {
	def unapply(x:Line) = Some((x.i,x.d))
  }

  implicit def stringWrapper(s :String):StringWrapper = new StringWrapper(s)
  implicit def intWrapper(i :Int):IntWrapper = new IntWrapper(i)
  object NilDoc extends Doc {
	override def toString = "Nil"
  }

}
