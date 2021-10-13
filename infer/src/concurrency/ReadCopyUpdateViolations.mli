(*
 * Copyright (c) 2020-present
 *
 * Daniel Marek (xmarek72@stud.fit.vutbr.cz)
 * Prof. Ing. Tomáš Vojnar Ph.D. (vojnar@fit.vut.cz)
 * Automated Analysis and Verification Research Group (VeriFIT)
 * Brno University of Technology, Czech Republic
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

val checker :
  ReadCopyUpdateDomain.summary InterproceduralAnalysis.t -> ReadCopyUpdateDomain.summary option

val printProblems : 
  ReadCopyUpdateDomain.summary InterproceduralAnalysis.file_t -> IssueLog.t 
