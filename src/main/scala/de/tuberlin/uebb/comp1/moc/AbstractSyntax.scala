/*
 * Copyright (c) 2013, TU Berlin
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *   * Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   * Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *   * Neither the name of the TU Berlin nor the
 *     names of its contributors may be used to endorse or promote products
 *     derived from this software without specific prior written permission.

 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL TU Berlin BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 */

package de.tuberlin.uebb.comp1.moc

/** Abstract syntax of μ-Opal */

object AbstractSyntax {
  /** A complete program */
  
  abstract class micropal ()
  case class Prog(d: Def,p:Position) extends micropal 

  abstract class Def () extends micropal
  case class Defm (b:Left, c:Expr, p:Position) extends Def
  case class List_Def(d1: Def, d2: Def,p:Position) extends Def
  
  abstract class Left () extends micropal
  case class Left_Main (a:Type,p:Position) extends Left
  case class Left_2 (a: String, b:Type,p:Position) extends Left
  case class Left_id (a: String, b: Type , c: Left,p:Position) extends Left
  case class Left_1_epsilon() extends Left
  case class List_Left (l1: Left, l2: Left,p:Position) extends Left

  abstract class Type () extends micropal
  case object TyNat extends Type
  case object TyBool extends Type
  case object TyUnknown extends Type
  case object TyUndef extends Type
  
  abstract class Expr () extends micropal
  case class num (v:String,p:Position) extends Expr
  case class True (v:String,p:Position) extends Expr
  case class False (v:String,p:Position) extends Expr
  case class Expr_id (s: String, e: List[Expr],p:Position) extends Expr
  case class Expr_epsilon () extends Expr
  //case class List_Expr(e1:Expr, e2:Expr,p:Position) extends Expr
  case class condt (e1:Expr, e2:Expr, e3:Expr,p:Position) extends Expr
  abstract class BoolT() extends micropal
  case class Tru() extends BoolT
  case class Fal() extends BoolT
}