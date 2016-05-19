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

import de.tuberlin.uebb.comp1.covm.instructions._
import scala.language.postfixOps

/** μ-Opal code generator */

object CodeGenerator {
	import AbstractSyntax._
	type Code = (List[Instruction])//, List[Instruction])
	/**
	 * Generates [[Code]] for the given list of definitions
	 * @param prog the context checked program [[Prog]]
	 * @param opts [[Options]] given as arguments to compiler
	 * @return Either an error message [[Diag]] or the code for the stack machine
	 */
	private type Env = Map[String, Int]
			private val emptyEnv = Map[String, Int]()
			private var nextLabelNum = 0
			var label_names: Map[String, Int] = Map()
			def compile(prog: Prog, opts: Options): Either[Diag, Code] = {
		cnt = 0
				label_names = Map()
				nextLabelNum = 0
				val (new_env, def_code) = start_def(prog.d)
				val fst_code = List(PushAddr(LabelledAddress("MAIN")), Call, Stop)
				val final_instr = patchLabels(fst_code ++ def_code ++ List(Ret))
				Right(final_instr)
	}
	private def patchLabels(instrs: List[Instruction]): List[Instruction] = {
		val (nolabel_instr) = removeLabels(instrs, 0)
				patchAddrs(nolabel_instr)
	}
	private def removeLabels(instrs: List[Instruction], count: Int): (List[Instruction]) =
			instrs match {
			case Nil => List()
			case Label(name) :: instrs1 => {
				label_names += (name -> count)
						removeLabels(instrs1, count)
			}
			case instr1 :: instrs1 => {
				val (instrs2) = removeLabels(instrs1, count + 1)
						(instr1 :: instrs2)
			}
	}
	private def patchAddrs(instrs: List[Instruction]): List[Instruction] =
			instrs.map(patchAddr())
			private def patchAddr()(instr: Instruction): Instruction =
			instr match {
			case Jz(LabelledAddress(name))  		=> 	Jz(Pointer(label_names(name)))
			case Jlt(LabelledAddress(name))	 		=> 	Jlt(Pointer(label_names(name)))
			case Jgt(LabelledAddress(name)) 		=> 	Jgt(Pointer(label_names(name)))
			case Jmp(LabelledAddress(name)) 		=> 	Jmp(Pointer(label_names(name)))
			case PushAddr(LabelledAddress(name)) 	=> 	PushAddr(Pointer(label_names(name)))
			case instr => instr
	}

	/** Generate code with labels */
	private def labelledCodeGen(Exp_env:Env, e: Expr): (Env, List[Instruction]) =
			e match {
			case num(s, _)   => {
				(shift(Exp_env), List(PushInt(s.toInt)))
			}
			case True(s, _)  => {
				(shift(Exp_env),List(PushInt(1)))
			}
			case False(s, _) => {
				(shift(Exp_env),List(PushInt(0)))
			}
			case Expr_id(a,e, _)=> {		
				val (exp_map, ilist) = labelledCodeGenExp(Exp_env, a, e)
						(exp_map, ilist)						
			}
			case condt(c, i, e, _) => {
				val (if_env, code_if) 	= 	labelledCodeGen(Exp_env, c)
						val (then_env, code_then) = 	labelledCodeGen(shiftM(if_env, 1), i)
						val (else_env, code_else) = 	labelledCodeGen(shiftM(then_env, 1), e)
						val true_label 	= 	freshLabel
						val false_label = freshLabel
						val condt_code = (code_if ++ List(Jz(LabelledAddress(false_label))) ++ code_then ++ List(Jmp(LabelledAddress(true_label))) ++ List(Label(false_label)) ++ code_else ++ List(Label(true_label)))
						(else_env, condt_code)
			}
			case _ => {
				(Exp_env, List(Abort("ERROR: No Return Argument found")))
			}
	}
	private def labelledCodeGenExp(func_env: Env, id: String, exp: List[Expr]) : (Env, List[Instruction]) = {
		var retList: List[Instruction] = List()
				val exList = exp
	id match {
	case "add" => {
		var vMap = func_env
				for (e <- exList) {
					val (tMap, inst) = labelledCodeGen(vMap, e)
							vMap = tMap
							retList = retList ++ inst
				}
		(shiftM(vMap,1), retList ++ List(Add))
	}
	case "sub" => {
		var vMap = func_env
				for (e <- exList) {
					val (tMap, inst) = labelledCodeGen(vMap, e)
							vMap = tMap
							retList = retList ++ inst
				}
		val negLabel = freshLabel
				val posLabel = freshLabel
				(shiftM(vMap,1), retList ++ List(Swap, Sub, Push(0), Jlt(LabelledAddress(negLabel)), Jmp(LabelledAddress(posLabel)),
						Label(negLabel), Abort("Sub Underflow Occurred"), Label(posLabel)))
	}
	case "mul" => {
		var vMap = func_env
				for (e <- exList) {
					val (tMap, inst) = labelledCodeGen(vMap, e)
							vMap = tMap
							retList = retList ++ inst
				}
		(shiftM(vMap,1), retList ++ List(Mul))
	}
	case "div" => {
		var vMap = func_env
				for (e <- exList) {
					val (tMap, inst) = labelledCodeGen(vMap, e)
							vMap = tMap
							retList = retList ++ inst
				}
		(shiftM(vMap,1), retList ++ List(Swap) ++ List(Div))
	}
	case "eq" => {
		var vMap = func_env
				for (e <- exList) {
					val (tMap, inst) = labelledCodeGen(vMap, e)
							vMap = tMap
							retList = retList ++ inst
				}
		val falseLabel = freshLabel
				val trueLabel = freshLabel
				(shiftM(vMap,1), retList ++
						List(Swap, Sub, Jz(LabelledAddress(trueLabel)), PushInt(0),
								Jmp(LabelledAddress(falseLabel)),
								Label(trueLabel), PushInt(1), Label(falseLabel)))
	}
	case "lt" => {
		var vMap = func_env
				for (e <- exList) {
					val (tMap, inst) = labelledCodeGen(vMap, e)
							vMap = tMap
							retList = retList ++ inst
				}
		val falseLabel = freshLabel
				val trueLabel = freshLabel
				(shiftM(vMap,1), retList ++
						List(Swap, Sub, Jlt(LabelledAddress(trueLabel)), PushInt(0),
								Jmp(LabelledAddress(falseLabel)),
								Label(trueLabel), PushInt(1), Label(falseLabel)))
	}
	case "and" => {
		var vMap = func_env
				for (e <- exList.reverse) {
					val (tMap, inst) = labelledCodeGen(vMap, e)
							vMap = tMap
							retList = retList ++ inst
				}
		(shiftM(vMap,1), retList ++ List(Mul))
	}
	case "or" => {
		var vMap = func_env
				for (e <- exList) {
					val (tMap, inst) = labelledCodeGen(vMap, e)
							vMap = tMap
							retList = retList ++ inst
				}
		val falseLabel = freshLabel
				val endLabel = freshLabel
				(shiftM(vMap,1), retList ++
						List(Add, Jz(LabelledAddress(falseLabel)), PushInt(1), Jmp(LabelledAddress(endLabel)),
								Label(falseLabel), PushInt(0), Label(endLabel)))
	}
	case "not" => {
		var vMap = func_env
				for (e <- exList) {
					val (tMap, inst) = labelledCodeGen(vMap, e)
							vMap = tMap
							retList = retList ++ inst
				}
		val falseLabel = freshLabel
				val endLabel = freshLabel
				(vMap, retList ++
						List(PushInt(1), Swap, Sub, Jz(LabelledAddress(falseLabel)), PushInt(1), Jmp(LabelledAddress(endLabel)),
								Label(falseLabel), PushInt(0), Label(endLabel)))
	}
	case _ => {
		if (func_env.contains(id)) {
			val tMap = shift(func_env)
					(tMap, List(Push(func_env(id))))

		}
		else {
			var vMap = func_env
					for (e <- exList) {
						val (tMap, inst) = labelledCodeGen(vMap, e)
								vMap = tMap
								retList = retList ++ inst
					}
			vMap = shiftM(vMap, exList.length)
					(shift(vMap), retList ++ List(PushAddr(LabelledAddress(id)), Call, Slide(exList.length)))
		}
	}
  }
}

	private def start_def(d : Def): (Env, List[Instruction]) = {
		d match {
		case Defm(l,e,_)=> {
			l match {
			case Left_id(a,b,c,_)=> {
				val (left_env) = eval_leftargs(c)
						cnt = 0
						val (exp_env1, exp_code1) = labelledCodeGen(left_env, e)
						(exp_env1,(List(Label(a)) ++ exp_code1 ++ List(Ret)))
			}
			case Left_Main(a, _) => {
				val (exp_env2, exp_code2) = labelledCodeGen(emptyEnv, e)
						(exp_env2,(List(Label("MAIN")) ++ exp_code2 ++ List(Ret)))
			}
			case _ => (emptyEnv,List())
			}         
		}
		case List_Def(d1,d2,_) => {
			val (env1, code1) = start_def(d1)
					val (env2,code2) = start_def(d2)
					(emptyEnv, code1 ++ code2)
		}
		}
	}  

	var cnt:Int=0
			private def eval_leftargs(l:Left): (Env) = {

		l match {

		case Left_2(str, t, _) => {	
			cnt = cnt + 1
					(Map(str -> cnt))
		}
		case List_Left(l1, l2, _) => {
			val (m1) = eval_leftargs(l1)
					val (m2) = eval_leftargs(l2)
					(m1++m2)
		}
		case Left_1_epsilon() => {
			(emptyEnv)
		}
		}
	}
	private def freshLabel: String = {
				val lab = "label" + nextLabelNum
						nextLabelNum = nextLabelNum + 1
						lab
		}	
		private def shift(env: Env): Env =
				env.mapValues(_ + 1)

				private def shiftM(env: Env, len: Int): Env =
				env.mapValues(_ - len)
}