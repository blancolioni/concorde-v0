with Ada.Containers.Doubly_Linked_Lists;

with Concorde.Colonies;
with Concorde.Worlds;

with Concorde.Logging;
with Concorde.Random;

with Concorde.Network;

with Concorde.Db.Colony;
with Concorde.Db.Network_Value;
with Concorde.Db.Node;
with Concorde.Db.Policy;
with Concorde.Db.Pop_Group;

package body Concorde.Managers.Colonies is

   type Pop_Group_Values is
      record
         Group       : Concorde.Db.Pop_Group_Reference;
         Income      : Concorde.Db.Network_Value_Reference;
         Tax_Rate    : Concorde.Db.Network_Value_Reference;
         Tax_Evasion : Concorde.Db.Network_Value_Reference;
         Happiness   : Concorde.Db.Network_Value_Reference;
      end record;

   package Pop_Group_Value_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Pop_Group_Values);

   type Root_Colony_Manager_Type is
     new Root_Manager_Type with
      record
         Colony      : Concorde.Db.Colony_Reference;
         Group_Nodes : Pop_Group_Value_Lists.List;
      end record;

   overriding function Identifier
     (Manager : Root_Colony_Manager_Type)
      return String
   is ("default-colony-manager");

   overriding procedure Activate
     (Manager : not null access Root_Colony_Manager_Type);

   --------------
   -- Activate --
   --------------

   overriding procedure Activate
     (Manager : not null access Root_Colony_Manager_Type)
   is
      Colony : constant Concorde.Db.Colony.Colony_Type :=
                 Concorde.Db.Colony.Get (Manager.Colony);

      function Get (Value : Concorde.Db.Network_Value_Reference)
                    return Real
      is (Concorde.Db.Network_Value.Get (Value).Real_Value);

   begin
      Concorde.Logging.Log
        (Actor    => Concorde.Worlds.Name (Colony.World),
         Location => "colony",
         Category => "managed",
         Message  => "activating");

      Concorde.Network.Update (Colony.Get_Network_Reference);

      for Group_Values of
        Manager.Group_Nodes
      loop
         Concorde.Colonies.Daily_Tax_Revenue
           (Colony  => Manager.Colony,
            Group   => Group_Values.Group,
            Rate    => Get (Group_Values.Tax_Rate),
            Income  => Get (Group_Values.Income),
            Evasion => Get (Group_Values.Tax_Evasion));
      end loop;

      for Policy of
        Concorde.Db.Policy.Scan_By_Tag
      loop
         Concorde.Colonies.Daily_Policy_Expense
           (Colony => Manager.Colony,
            Policy => Policy.Get_Policy_Reference,
            Value  =>
              Concorde.Network.Current_Value
                (Colony.Get_Network_Reference, Policy.Tag));
      end loop;

      Manager.Set_Next_Update_Delay (Concorde.Calendar.Days (1));

   end Activate;

   ----------------------------
   -- Create_Default_Manager --
   ----------------------------

   function Create_Default_Manager
     (Managed : Concorde.Db.Managed_Reference) return Manager_Type
   is
      use Concorde.Calendar;
      Colony  : constant Concorde.Db.Colony.Colony_Type :=
                  Concorde.Db.Colony.Get_Colony (Managed);
      Manager : Root_Colony_Manager_Type :=
                  Root_Colony_Manager_Type'
                    (Managed         => Managed,
                     Is_Active       => True,
                     Has_Next_Update => True,
                     Next_Update     =>
                       Concorde.Calendar.Clock
                     + Concorde.Calendar.Days (Concorde.Random.Unit_Random),
                     Colony          => Colony.Get_Colony_Reference,
                     Group_Nodes     => Pop_Group_Value_Lists.Empty_List);

      procedure Add_Group
        (Name : String);

      ---------------
      -- Add_Group --
      ---------------

      procedure Add_Group
        (Name : String)
      is
         function Get
           (Suffix : String)
            return Concorde.Db.Network_Value_Reference;

         ---------
         -- Get --
         ---------

         function Get
           (Suffix : String)
            return Concorde.Db.Network_Value_Reference
         is
            use type Concorde.Db.Node_Reference;
            Tag : constant String :=
                    Name & (if Suffix = "" then "" else "-" & Suffix);
            Node : constant Concorde.Db.Node_Reference :=
                     Concorde.Db.Node.Get_Reference_By_Tag (Tag);
         begin
            pragma Assert (Node /= Concorde.Db.Null_Node_Reference);
            return Concorde.Db.Network_Value.Get_Reference_By_Network_Value
              (Colony.Get_Network_Reference, Node);
         end Get;

         Group : constant Pop_Group_Values := Pop_Group_Values'
           (Group       => Concorde.Db.Pop_Group.Get_Reference_By_Tag (Name),
            Income      => Get ("income"),
            Tax_Rate    => Get ("income-tax-rate"),
            Tax_Evasion => Get ("tax-evasion"),
            Happiness   => Get (""));
      begin
         Manager.Group_Nodes.Append (Group);
      end Add_Group;

   begin
      for Group of
        Concorde.Db.Pop_Group.Select_By_Is_Wealth_Group (True)
      loop
         Add_Group (Group.Tag);
      end loop;
      return new Root_Colony_Manager_Type'(Manager);
   end Create_Default_Manager;

end Concorde.Managers.Colonies;
