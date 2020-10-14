package edu.colorado.csci3155.project1


/* -- Here are all the instructions to be supported --*/
sealed trait StackMachineInstruction
case class LoadI(s: String) extends StackMachineInstruction
case class  StoreI(s: String) extends StackMachineInstruction
case class PushI(f: Double) extends StackMachineInstruction
case object AddI extends StackMachineInstruction
case object SubI extends StackMachineInstruction
case object MultI extends StackMachineInstruction
case object DivI extends StackMachineInstruction
case object ExpI extends StackMachineInstruction
case object LogI extends StackMachineInstruction
case object SinI extends StackMachineInstruction
case object CosI extends StackMachineInstruction
case object PopI extends StackMachineInstruction


object StackMachineEmulator {



    /* Function emulateSingleInstruction
        Given a list of doubles to represent a stack
              a map from string to double precision numbers for the environment
        and   a single instruction of type StackMachineInstruction
        Return a tuple that contains the
              modified stack that results when the instruction is executed.
              modified environment that results when the instruction is executed.

        Make sure you handle the error cases: eg., stack size must be appropriate for the instruction
        being executed. Division by zero, log of a non negative number
        Throw an exception or assertion violation when error happens.
     */
    def emulateSingleInstruction(stack: List[Double],
                                 env: Map[String, Double],
                                 ins: StackMachineInstruction): (List[Double], Map[String, Double]) = {
        // TODO: Your code here.
        ins match {
            case LoadI(s) => {
                if (stack.length < 1)
                    throw new IllegalArgumentException("Not enough elements on stack")
                (stack.tail, env+ (s->stack.head))
            }
            case StoreI(s) => {
                if (env.contains(s)){
                    (env(s)::stack,env)
                }
                else{
                    throw new IllegalArgumentException("Not found identifier")
                }
            }
            case PushI(d)=> {
                (d::stack, env)
            }
            case PopI => {
                if (stack.length == 0) {
                    throw new IllegalArgumentException("Stack empty")
                }
                (stack.tail, env)
            }
            case AddI => {
                if (stack.length < 2){
                    throw new IllegalArgumentException("Not found identifier")
                }
                val a = stack.head
                val Nstack = stack.tail
                (a+Nstack.head :: Nstack.tail, env)
            }
            case SubI =>{
                if (stack.length < 2){
                    throw new IllegalArgumentException("Not found identifier")
                }
                val a = stack.head
                val Nstack = stack.tail
                (Nstack.head-a :: Nstack.tail, env)
            }
            case MultI=> {
                if (stack.length < 2){
                    throw new IllegalArgumentException("Not found identifier")
                }
                val a = stack.head
                val Nstack = stack.tail
                (Nstack.head * a :: Nstack.tail, env)
            }
            case DivI=> {
                if (stack.length < 2){
                    throw new IllegalArgumentException("Not found identifier")
                }
                val a = stack.head
                val Nstack = stack.tail
                (Nstack.head / a :: Nstack.tail, env)
            }
            case LogI=> {
                if (stack.length < 1) {
                    throw new IllegalArgumentException("Not found identifier")
                }
                (math.log(stack.head)::stack.tail, env)
            }
            case ExpI=> {
                if (stack.length < 1) {
                    throw new IllegalArgumentException("Not found identifier")
                }
                (math.exp(stack.head)::stack.tail, env)
            }
            case SinI => {
                if (stack.length < 1) {
                    throw new IllegalArgumentException("Not found identifier")
                }
                (math.sin(stack.head)::stack.tail, env)
            }
            case CosI => {
                if (stack.length < 1) {
                    throw new IllegalArgumentException("Not found identifier")
                }
                (math.cos(stack.head)::stack.tail, env)
            }

        }
    }

    /* Function emulateStackMachine
       Execute the list of instructions provided as inputs using the
       emulateSingleInstruction function.
       Use foldLeft over list of instruction rather than a for loop if you can.
       Return value must be the final environment.

       Hint: accumulator for foldLeft must be a tuple (List[Double], Map[String,Double])
             initial value of this accumulator must be (Nil, Map.empty)
             You should use emulateSingleInstruction to update the accmulator.
             It will all fit nicely once you figure it out.
     */
    def emulateStackMachine(instructionList: List[StackMachineInstruction]): Map[String, Double] =
    {
        val f = instructionList.foldLeft[(List[Double], Map[String,Double])](Nil, Map.empty){
            (t,i) => emulateSingleInstruction(t._1,t._2, i)
        }
        f._2
    }
}