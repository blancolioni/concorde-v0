with Concorde.Expressions;
with Concorde.Nodes;

package Concorde.Evaluator is

   type Evaluation_Handle is private;

   function Compile
     (Expression : Concorde.Expressions.Expression_Type)
      return Evaluation_Handle;

   function Evaluate
     (Handle  : Evaluation_Handle;
      Network : Concorde.Nodes.Value_Container)
      return Real;

private

   type Evaluation_Handle is new Positive;

end Concorde.Evaluator;
