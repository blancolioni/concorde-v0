private with WL.String_Maps;

with Concorde.Values.Vectors;

package Concorde.Primitives is

   type Primitive_Interface is interface;

   function Evaluate
     (Primitive   : Primitive_Interface;
      Arguments   : Concorde.Values.Vectors.Vector)
      return Concorde.Values.Value_Interface'Class
      is abstract;

   function Minimum_Argument_Count
     (Primitive : Primitive_Interface)
      return Natural
      is abstract;

   function Maximum_Argument_Count
     (Primitive : Primitive_Interface)
      return Natural
      is abstract;

   function Evaluate
     (Name        : String;
      Arguments   : Concorde.Values.Vectors.Vector)
      return Concorde.Values.Value_Interface'Class;

private

   type Binary_Primitive is
     abstract new Primitive_Interface with null record;

   function Evaluate
     (Primitive   : Binary_Primitive;
      Left, Right : Concorde.Values.Value_Interface'Class)
      return Concorde.Values.Value_Interface'Class
      is abstract;

   overriding function Evaluate
     (Primitive   : Binary_Primitive;
      Arguments   : Concorde.Values.Vectors.Vector)
      return Concorde.Values.Value_Interface'Class;

   overriding function Minimum_Argument_Count
     (Primitive : Binary_Primitive)
      return Natural
   is (2);

   overriding function Maximum_Argument_Count
     (Primitive : Binary_Primitive)
      return Natural
      is (2);

   package Primitive_Maps is
     new WL.String_Maps (Primitive_Interface'Class);

   Map : Primitive_Maps.Map;

end Concorde.Primitives;
