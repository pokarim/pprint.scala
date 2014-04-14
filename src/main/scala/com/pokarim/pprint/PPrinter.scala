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

object PPrinter {
  import scala.language.implicitConversions
  import ColorUtil._
  import Stream._
  import Docs._
  def group (x:DOC):DOC = flatten(x) :<|> x

  def flatten: DOC => DOC = {
	case NIL => NIL
	case d: :<>  => new :<>(flatten(d.l),flatten(d.r))
	case NEST(i,x) => NEST(i,flatten(x))
	case d: TEXT => d
	case LINE => TEXT(" ")
	case d: :<|>  => flatten(d.l)}

  def layout(d:Doc) = layoutStream(d).mkString("")
  def layoutStream : Doc=>Stream[String] = {
	case NilDoc => empty
	case s Text x => s #:: layoutStream(x)
	case i Line x => "\n" #:: (" " * i) #:: layoutStream(x)}
  
  def best(w: Int, k: Int, x: DOC):Doc = be(w,k,(0,x)#::empty)

  def be(w: Int, k: Int, xs: Stream[(Int,DOC)]):Doc = xs match {
   	case Empty => NilDoc
   	case ((i,NIL) #:: z) => be(w,k,z)
   	case ((i, d: :<>)#::z) => be(w,k,(i,d.l) #:: (i,d.r) #:: z )
   	case ((i,NEST(j,x))#::z) => be( w, k,((i+j, x) #:: z))
   	case ((i,TEXT(s))#::z) => s Text be(w, (k + getLen(s)), z)
   	case ((i,LINE)#::z) => i Line be( w, i, z)
   	case ((i,d: :<|>)#::z) => 
   	  better( w, k,be(w, k,(i,d.l) #:: z), be(w,k,(i,d.r) #:: z))
   }
  
  def better(w: Int, k: Int, x: =>Doc, y: =>Doc):Doc = if (fits(w-k,x)) x else y

  def fits(w:Int,x: =>Doc):Boolean = 
	w >= 0 && (x match {
	  case NilDoc => true
	  case s Text x => fits( w - getLen(s),x)
	  case _: Line => true
	})
  def pretty(w:Int, x:DOC) = layout(best (w, 0, x))
  def labeledBracket2(left:String,body:DOC,right:String) = {
	NEST(getLen(left), TEXT(left) <> body <>TEXT(right))
  }
  def labeledBracket(left:String,body:DOC,right:String) = 
	group(NEST(getLen(left), TEXT(left) <> body <>TEXT(right)))

  def labeledBracketWC[A <% DOC](left:String,body:Seq[A],right:String,c:DOC=TEXT(",")) =
	labeledBracket(left, joinWComma(body.map((x:A) => x:DOC), c), right)

  def labeledBracketWC2[A <% DOC](left:String,body:Seq[A],right:String,c:DOC=TEXT(",")) =
	labeledBracket2(left, joinWComma(body.map((x:A) => x:DOC), c), right)

  def labeledRBracketWC[A <% DOC](left:String,body:Seq[A],col:Int=7) =
	labeledBracketWC(withColor(col,left+ "(") , body, 
					 withColor(col,")"),TEXT(withColor(col,",")))

  def labeledRBracketWC2[A <% DOC](left:String,body:Seq[A],col:Int=7) =
	labeledBracketWC2(withColor(col,left+ "(") , body, 
					 withColor(col,")"),TEXT(withColor(col,",")))


  def joinWComma(xs: Seq[DOC],c:DOC=TEXT(",")):DOC = 
	(c <> LINE).join(xs)
  def folddoc(f:(=>DOC,=>DOC) => DOC, xs:Seq[DOC]):DOC = xs match{
	case _ if xs.isEmpty => NIL
	case _ if xs.tail.isEmpty => xs.head
	case _ => f(xs.head,folddoc(f,xs.tail))
  }

  def fill(xs:scala.collection.GenTraversableOnce[DOC]):DOC = _fill(xs.toStream)
  def _fill(xs:Stream[DOC]):DOC = xs match{
	case Empty if xs.isEmpty => NIL
	case x #:: Empty => x
	case x #:: y #:: zs => 
	  (flatten(x) <+> _fill(flatten(y) #:: zs)) :<|> (x </> _fill(y #:: zs))
  }
  
  def spread(xs:Seq[DOC]):DOC = folddoc((_ <+> _), xs)
  def stack(xs:Seq[DOC]):DOC = folddoc((_ </> _), xs)

  
  object NIL extends DOC {
	override def toString = "NIL"
  }
  case class NEST(n:Int, d:DOC) extends DOC
  case class TEXT(s:String) extends DOC
  object LINE extends DOC{
	override def toString = "LINE"
  }
  
  class DOCwrapper(l : =>DOC){
    def <>(r : => DOC) = new :<>(l,r)
	def <+>(r : => DOC) = l <> TEXT(" ") <> r
	def </>(r : => DOC) = l <> LINE <> r
    def :<>(r : => DOC) = new :<>(l,r)
    def :<|>(r : => DOC) = new :<|>(l,r)
  }

  implicit def docWrapper(l: => DOC): DOCwrapper = new DOCwrapper(l)

  class :<> (_l: => DOC, _r: =>DOC) extends DOC {
	def l = _l
	def r = _r
  }

  class :<|> (_l: => DOC, _r: =>DOC) extends DOC{
	def l = _l
	def r = _r
  }

}
