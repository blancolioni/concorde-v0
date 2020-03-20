private with Ada.Containers.Indefinite_Vectors;

package Concorde.Behaviors.Networks is

   type Behavior_Id is private;
   No_Behavior : constant Behavior_Id;

   type Root_Network_Type is tagged private;

   function New_Behavior
     (Network       : in out Root_Network_Type'Class;
      Name          : String;
      Initial_Value : Real)
      return Behavior_Id;

   procedure Set_Behavior
     (Network : in out Root_Network_Type'Class;
      Id      : Behavior_Id;
      Behavior  : Behavior_Type'Class);

   function Get_Behavior
     (Network : Root_Network_Type'Class;
      Id      : Behavior_Id)
      return Behavior_Type'Class;

   function Get_Behavior
     (Network : Root_Network_Type'Class;
      Name    : String)
      return Behavior_Id;

   procedure Update_Behavior
     (Network : in out Root_Network_Type'Class;
      Id      : Behavior_Id;
      Update  : not null access
        function (Behavior : Behavior_Type'Class) return Behavior_Type'Class);

   type Network_Type is access all Root_Network_Type'Class;

   function Network_Behavior
     (Network : not null access Root_Network_Type'Class;
      Id      : Behavior_Id)
      return Behavior_Type'Class;

private

   type Behavior_Id is new Natural;
   No_Behavior : constant Behavior_Id := 0;

   subtype Real_Behavior_Id is Behavior_Id range 1 .. Behavior_Id'Last;

   package Behavior_Vectors is
     new Ada.Containers.Indefinite_Vectors (Real_Behavior_Id, Behavior_Type'Class);

   type Root_Network_Type is
     new Behavior_Vectors.Vector with null record;

end Concorde.Behaviors.Networks;
