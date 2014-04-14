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

import scala.annotation.tailrec
import scala.collection.immutable

import PPrinter._
import ColorUtil._

trait DOC {
  def toPretty(w:Int):String = pretty(w, this)
  def count(w:Int):Int = layoutStream(best(w,0,this)).length
  def join(xs: Seq[DOC]) = folddoc((_ <> this <> _),xs)

}

object DOC{
  import scala.collection.mutable.ArrayBuffer
  import scala.language.implicitConversions
  implicit def Option2Doc[A <% DOC](x:Option[A]):DOC = x match {
	case None => TEXT("None")
	case Some(a) => labeledRBracketWC("Some", List(a:DOC))
  }
  implicit def String2Doc(x:String):DOC = TEXT(withColor(2, "\"%s\"" ,x))
  implicit def Any2Doc(x:Any):DOC = TEXT(x.toString)
  implicit def Pair2Doc[A <% DOC,B <% DOC](x:(A,B)) = x match{
	case (x, y) => group((x:DOC) <> LINE <> NEST(3, TEXT(
	  withColor(builtinColor,"-> ")) <> y))
  }

  implicit def Tup32Doc[A <% DOC,B <% DOC,C <% DOC](x:(A,B,C)) = x match{
	case (x, y, z) => 
	  group(
	 	(x:DOC) <>  NEST(3, LINE <> 
						 TEXT(withColor(builtinColor,"-> "))
						 <> Pair2Doc((y,z)) ))
  }
  
  implicit def Map2Doc[A <% DOC,B <% DOC](xs:immutable.Map[A,B]):DOC =
	labeledRBracketWC("Map", xs.toSeq,builtinColor)
  implicit def List2Doc[A <% DOC](xs:List[A]):DOC = 
	labeledRBracketWC("List", xs,builtinColor)
  implicit def Set2Doc[A <% DOC](xs:collection.Set[A]):DOC = 
	labeledRBracketWC("Set", xs.toSeq,builtinColor)
  implicit def ArrayBuffer2Doc[A <% DOC](xs:ArrayBuffer[A]):DOC = 
	labeledRBracketWC("ArrayBuffer", xs,builtinColor)
  implicit def Stream2Doc[A <% DOC](xs:Stream[A]):DOC = 
	labeledRBracketWC("Stream", xs,builtinColor)

  val builtinColor = 12//75

  implicit def Seq2Doc[A <% DOC](xs:Seq[A]):DOC = 
	
	(xs : @unchecked) match {
	  case xs : ArrayBuffer[_] => ArrayBuffer2Doc[A](xs.asInstanceOf[ArrayBuffer[A]])
	  case xs : List[_] => List2Doc[A](xs.asInstanceOf[List[A]])
	  case _ :Stream[_] => Stream2Doc[A](xs.asInstanceOf[Stream[A]])
	  case _ => labeledRBracketWC(xs.getClass.getName, xs)

	}

}
