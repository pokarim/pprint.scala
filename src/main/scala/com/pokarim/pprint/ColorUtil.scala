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
 
object ColorUtil{ 
  def withColor(c:scala.Int,s:String,args: Any*) = {
	if (!enableColor.value) s
	else if (c < 8) "\033[3%sm%s\033[39m" format(c,s) format(args : _*)
	else "\033[38;5;%sm%s\033[39m" format(c,s) format(args : _*)

  }
  def getLen(s:String) = s.replaceAll(
	"\033\\[[^m]+m",""
  ).length
}
