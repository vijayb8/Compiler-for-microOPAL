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

 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS """AS IS""" AND
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
 * */

package de.tuberlin.uebb.comp1.moc

/** μ-Opal interpreter */

object Interpreter {

	import AbstractSyntax._
	import de.tuberlin.uebb.comp1.moc.{ Value => V }
	//import scala.collection.mutable.Map
	/**
	 * Interprets the given list of definitions [[Def]] starting with main function
	 * @param prog the context checked program [[Prog]]
	 * @param opts [[Options]] given as arguments to compiler
	 * @return Either an error message [[Diag]] or a [[Value]]
	 */

	var Interpreter_map:Map[String, Expr]=Map()
	var Interpreter_map_args : Map[String, List[String]] = Map()
	var list_str:List[String] = List()
	val Int_list_prim_func:List[String] = List("add", "sub", "div", "mul", "eq", "lt", "and", "or", "not")
	
	def interpret(prog: Prog, opts: Options): Either[Diag, Value] = {
			Initialize_Int()
			start_def(prog.d)
			val ft=startProg(prog)
			if(ft.isLeft)
				Left(ft.left.get)
				else {
				  Right(ft.right.get)
				}
	}

	private def Initialize_Int() = {
	  Interpreter_map = Map()
	  Interpreter_map_args = Map()
	  list_str = List()
	}
	
	private def startProg(prog:Prog): Either[Diag, V] = {
			val small_list_args:List[V] = List()
					val v1=eval_expr(Interpreter_map("MAIN"),"MAIN", small_list_args)
					if (v1.isLeft)
						Left(v1.left.get)
						else
							Right(v1.right.get)
	}

	val error: Map[String, List[Diag]] = Map("error" -> List())
			private def primitive_func (str: String, list_val: List[V], p:Position): Either[Diag, V] = str match {
			case "add" => {
				val i1=list_val(0) match {
				case NumV(x)=> x.toLong 
				}
				val i2=list_val(1) match {
				case NumV(x) => x.toLong
				}
				val l = i1 + i2
						val l_shift = l >> 32
						if(l_shift.==(0))
						{
							Right(NumV(l.toInt))
						}
						else
						{
							Left(Diag("Overflow in addition at primitive function add ", p))
						}
			}
			case "sub" => {
				val i1=list_val(0) match {
				case NumV(x)=> x.toLong
				}
				val i2=list_val(1) match {
				case NumV(x)=> x.toLong
				}
				val l= i1 - i2
						val l_shift = l >> 32
						if(l_shift.==(0)) 
						{
							Right(NumV(l.toInt))
						}
						else
						{
							Left(Diag("Underflow condition occurs at primitive function - sub ", p))
						}
			}
			case "div" => {
				val i1=list_val(0) match {
				case NumV(x)=> x.toLong
				}
				val i2=list_val(1) match {
				case NumV(x)=> x.toLong
				}
				if(i2.!=(0))
				{
					val l = i1/i2
							Right(NumV(l.toInt))
				}
				else
				{
					Left(Diag("Found Division by zero at primitive function div ", p)) 
				}
			}
			case "mul" => {
				val i1=list_val(0) match {
				case NumV(x) => x.toLong
				}
				val i2=list_val(1) match {
				case NumV(x) => x.toLong
				}
				val l = i1 * i2 
						val l_shift = l >> 32
						if(l_shift.==(0))
						{
							Right(NumV(l.toInt))
						}
						else
						{ Left(Diag("Overflow condition occurs at the primitive function - mul ", p))
						}

			}
			case "eq" => { 
				val i1=list_val(0) match {
				case NumV(x)=> x
				}
				val i2=list_val(1) match {
				case NumV(x)=>x
				}
				if(i1.!=(i2))
				{   Right(BoolV(false))
				}
				else
				{   Right(BoolV(true))
				}

			}
			case "lt" => {
				val i1=list_val(0) match {
				case NumV(x)=> x
				}
				val i2=list_val(1) match {
				case NumV(x)=>x
				}
				if(i1<i2)
				{
					Right(BoolV(true))
				}
				else
				{
					Right(BoolV(false))
				}
			}
			case "and" => {
				val i1=list_val(0) match {
				case BoolV(x)=>x
				}
				val i2=list_val(1) match {
				case BoolV(x)=>x
				}
				if(i1==true&&i2==true)
				{
					Right(BoolV(true))
				}
				else
				{
					Right(BoolV(false))
				}
			}
			case "or" => {
				val i1=list_val(0) match {
				case BoolV(x)=>x
				}
				val i2=list_val(1) match {
				case BoolV(x)=>x
				}
				if(i1==false&&i2==false)
				{
					Right(BoolV(false))
				}
				else
				{
					Right(BoolV(true))
				}

			}
			case "not" => {
				val i1=list_val(0) match {
				case BoolV(x)=>x
				}
				if(i1==true)
				{
					Right(BoolV(false))
				}
				else
					Right(BoolV(true))
			}
						}

    private def eval_expr(e:Expr, str_id:String, list_val_expr: List[Value]): Either[Diag, V]= e match {
	case num(s, _) => Right(NumV(s.toInt))
	case True(s, _) => Right(BoolV(true))
	case False(s, _) => Right(BoolV(false))
	case Expr_id(a,e,p)=> {		
		var list_val:List[V] = List()
		if (Interpreter_map_args(str_id).contains(a)) {
					val listN = Interpreter_map_args(str_id)
							val index: Int = listN.indexOf(a)
							Right(list_val_expr(index))
				}
				else {
					for (e1 <- e){					  
						val v_expr = eval_expr(e1, str_id, list_val_expr)
								if (v_expr.isLeft){
								  return Left(v_expr.left.get) 									
								  }
									else {
										list_val ::= v_expr.right.get
									}
					}
					if (Int_list_prim_func.contains(a)){ 
						val v_prim = primitive_func(a, list_val, p)		
								if (v_prim.isLeft) {
								  list_val = List()
									Left(v_prim.left.get)
								}
									else {
									  list_val = List()
										Right(v_prim.right.get)
									}
					}
					else { 
						val id_expr = Interpreter_map(a) 
								val v_def = eval_expr(id_expr, a, list_val)
								if (v_def.isLeft) {
								  list_val = List()
								  Left(v_def.left.get)
								}
									else {
									  list_val = List()
									  Right(v_def.right.get)
									}

					}}
	}

	case condt(c, i, e, p) => {
		val cval = eval_expr(c, str_id, list_val_expr)
				if (cval.isLeft) {
				  Left(cval.left.get)
				  }
				else {
					cval.right.get match {
					case BoolV(true) => {
						val v_if = eval_expr(i, str_id, list_val_expr)
								if (v_if.isRight)
								Right(v_if.right.get)
								else
								  Left(v_if.left.get)
					}
					case BoolV(false) => {
						e match {
						case Expr_epsilon() => Left(Diag("Else case missing in the conditional expression ", p))

						case _ => {
							val v_else = eval_expr(e, str_id, list_val_expr)
									if (v_else.isLeft) {
									Left(v_else.left.get)
									}
									else
									  Right(v_else.right.get)
						}
						}}
					}
					}					
	}}

			private def eval_leftargs(l:Left): List[String] = l match {
			case Left_2(str, t, _) => {
				List(str)
			}
			case List_Left(l1, l2, _) => {
				val s1 = eval_leftargs(l1)
						val s2 = eval_leftargs(l2)
						list_str:::s1:::s2
			}
			case Left_1_epsilon() => {
				List()
			}
			}

			private def start_def(d : Def):Option[Value] = {
					d match {
					case Defm(l,e,_)=> {
						l match {
						case Left_id(a,b,c,_)=> {
							val str_l = eval_leftargs(c)
									Interpreter_map_args += (a -> str_l)
									list_str = List()
									Interpreter_map += (a -> e)
									None
						}
						case Left_Main(a, _) => {
							Interpreter_map += ("MAIN" -> e) 
									Interpreter_map_args += ("MAIN" -> List())
									None
						}
						case _ => None
						}         
					}
					case List_Def(d1,d2,_) => {
						start_def(d1)
						start_def(d2)
						None
					}
					}
			}  



}
