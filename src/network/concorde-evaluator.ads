with Concorde.Expressions;
with Concorde.Network;

package Concorde.Evaluator is

   type Evaluation_Handle is private;

   function Compile
     (Expression : Concorde.Expressions.Expression_Type;
      Current    : Concorde.Network.Network_Node_Type)
      return Evaluation_Handle;

   function Evaluate
     (Handle  : Evaluation_Handle;
      Network : Concorde.Network.Network_Type)
      return Real;

private

   type Evaluation_Handle is new Positive;

end Concorde.Evaluator;
