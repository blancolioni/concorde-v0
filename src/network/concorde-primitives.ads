private with WL.String_Maps;

with Concorde.Values.Vectors;

package Concorde.Primitives is

   type Operator_Type is ('+', '-', '*', '/', '^');

   type Primitive_Interface is interface;

   function Evaluate
     (Primitive   : Primitive_Interface;
      Arguments   : Concorde.Values.Vectors.Vector)
      return Concorde.Values.Value_Interface'Class
      is abstract;

   function Argument_Count
     (Primitive : Primitive_Interface)
      return Natural
      is abstract;

   function Name
     (Primitive : Primitive_Interface)
      return String
      is abstract;

   function Evaluate
     (Name        : String;
      Arguments   : Concorde.Values.Vectors.Vector)
      return Concorde.Values.Value_Interface'Class;

   function Is_Primitive
     (Name        : String)
      return Boolean;

   function Get_Primitive
     (Name        : String)
      return Primitive_Interface'Class
     with Pre => Is_Primitive (Name);

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

   overriding function Argument_Count
     (Primitive : Binary_Primitive)
      return Natural
   is (2);

   package Primitive_Maps is
     new WL.String_Maps (Primitive_Interface'Class);

   Map : Primitive_Maps.Map;

   function Is_Primitive
     (Name        : String)
      return Boolean
   is (Map.Contains (Name));

   function Get_Primitive
     (Name        : String)
      return Primitive_Interface'Class
   is (Map.Element (Name));

end Concorde.Primitives;
