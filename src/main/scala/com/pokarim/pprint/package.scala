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

object `package`{
  import scala.util.DynamicVariable

  val enableColor = new DynamicVariable[Boolean](true)
  val enablePrint = new DynamicVariable[Boolean](true)
  var lineWidth = new DynamicVariable[Int](90)

  import PPrinter._
  //import scala.collection

  def toPrettyString(xs : DOC*):String = {
	val d = fill(xs)
	d.toPretty(lineWidth.value)
  }

  def pprint(xs : DOC*):Unit = if (enablePrint.value){
	print(toPrettyString(xs:_*))
  }


  def pprintln(xs : DOC*):Unit = if (enablePrint.value){
	println(toPrettyString(xs:_*))
  }

  def pprn(xs : DOC*):Unit = pprintln(xs:_*)

  def pprn1[A <% DOC ](x : A): A = {pprn(x);x}
  def pprn1[A <% DOC,B <% DOC ](x : A,y :B): B = {pprn(x,y);y}

}
