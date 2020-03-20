with Ada.Containers.Doubly_Linked_Lists;

with Concorde.Colonies;
with Concorde.Worlds;

with Concorde.Logging;
with Concorde.Money;
with Concorde.Random;
with Concorde.Real_Images;

with Concorde.Network;

with Concorde.Db.Colony;
with Concorde.Db.Colony_Policy;
with Concorde.Db.Network_Value;
with Concorde.Db.Node;
with Concorde.Db.Policy;
with Concorde.Db.Pop;
with Concorde.Db.Pop_Group;
with Concorde.Db.Pop_Group_Member;

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

   function Image (X : Real) return String
                   renames Concorde.Real_Images.Approximate_Image
     with Unreferenced;

   --------------
   -- Activate --
   --------------

   overriding procedure Activate
     (Manager : not null access Root_Colony_Manager_Type)
   is
      Colony : constant Concorde.Db.Colony.Colony_Type :=
                 Concorde.Db.Colony.Get (Manager.Colony);

      procedure Update_Population_Sizes;

      procedure Update_Population_Sizes is

         package Pop_Group_Maps is
           new WL.String_Maps (Real);

         Group_Size : Pop_Group_Maps.Map;

         procedure Add_Pop
           (Group : Concorde.Db.Pop_Group_Reference;
            Size  : Non_Negative_Real);

         -------------
         -- Add_Pop --
         -------------

         procedure Add_Pop
           (Group : Concorde.Db.Pop_Group_Reference;
            Size  : Non_Negative_Real)
         is
            Tag : constant String :=
                    Concorde.Db.Pop_Group.Get (Group).Tag;
         begin
            if not Group_Size.Contains (Tag) then
               Group_Size.Insert (Tag, 0.0);
            end if;
            Group_Size (Tag) := Group_Size (Tag) + Size;
         end Add_Pop;

      begin
         for Pop_Group of Concorde.Db.Pop_Group.Scan_By_Tag loop
            Group_Size.Insert (Pop_Group.Tag, 0.0);
         end loop;

         for Pop of Concorde.Db.Pop.Select_By_Colony (Manager.Colony) loop
            for Group of
              Concorde.Db.Pop_Group_Member.Select_By_Pop
                (Pop.Get_Pop_Reference)
            loop
               Add_Pop (Group.Pop_Group, Pop.Size);
            end loop;
         end loop;

         for Pop_Group of Concorde.Db.Pop_Group.Scan_By_Tag loop
            Concorde.Network.Set_New_Value
              (Colony.Get_Network_Reference, Pop_Group.Tag & "-population",
               Group_Size (Pop_Group.Tag));
            Concorde.Network.Commit_New_Value
              (Colony.Get_Network_Reference, Pop_Group.Tag & "-population");
            Concorde.Logging.Log
              (Actor    => Pop_Group.Tag,
               Location => Concorde.Worlds.Name (Colony.World),
               Category => "size",
               Message  =>
                 Natural'Image
                   (Natural (Group_Size.Element (Pop_Group.Tag))));
         end loop;

      end Update_Population_Sizes;

   begin
      Concorde.Logging.Log
        (Actor    => Concorde.Worlds.Name (Colony.World),
         Location => "colony",
         Category => "managed",
         Message  => "activating");

      Update_Population_Sizes;

      Concorde.Network.Update (Colony.Get_Network_Reference);

      for Policy of
        Concorde.Db.Policy.Scan_By_Tag
      loop
         Concorde.Db.Colony_Policy.Update_Colony_Policy
           (Concorde.Db.Colony_Policy.Get_Reference_By_Colony_Policy
              (Manager.Colony, Policy.Get_Policy_Reference))
           .Set_Revenue (Concorde.Money.Zero)
           .Set_Expense (Concorde.Money.Zero)
           .Done;
      end loop;

      for Pop of Concorde.Db.Pop.Select_By_Colony (Manager.Colony) loop
         Concorde.Colonies.Daily_Tax_Revenue
           (Manager.Colony, Pop.Get_Pop_Reference);
      end loop;

--
--        for Group_Values of
--          Manager.Group_Nodes
--        loop
--           Concorde.Colonies.Daily_Tax_Revenue
--             (Colony  => Manager.Colony,
--              Group   => Group_Values.Group);
--              Rate    => Get (Group_Values.Tax_Rate),
--              Income  => Get (Group_Values.Income),
--              Evasion => Get (Group_Values.Tax_Evasion));
--        end loop;

      for Policy of
        Concorde.Db.Policy.Scan_By_Tag
      loop
         Concorde.Colonies.Daily_Policy_Expense
           (Colony => Manager.Colony,
            Policy => Policy.Get_Policy_Reference);
      end loop;

      declare
         Birth_Rate : constant Unit_Real :=
                        Concorde.Network.Current_Value
                          (Colony.Get_Network_Reference, "birth-rate");
         Death_Rate : constant Unit_Real :=
                        Concorde.Network.Current_Value
                          (Colony.Get_Network_Reference, "death-rate");
      begin
         for Pop of
           Concorde.Db.Pop.Select_By_Colony (Manager.Colony)
         loop
            declare
               Births : constant Non_Negative_Real :=
                          Pop.Size * (Birth_Rate / 360.0);
               Deaths : constant Non_Negative_Real :=
                          Pop.Size * (Death_Rate / 360.0);
               New_Size : constant Non_Negative_Real :=
                            Real'Max (Pop.Size + Births - Deaths, 0.0);
            begin
--                 Concorde.Logging.Log
--                   (Actor    => "colony",
--                    Location => Concorde.Worlds.Name (Colony.World),
--                    Category => "pop changes",
--                    Message  =>
--                      "old=" & Image (Pop.Size)
--                    & "; births=" & Image (Births)
--                    & "; deaths=" & Image (Deaths)
--                    & "; new size=" & Image (New_Size));
               Concorde.Db.Pop.Update_Pop (Pop.Get_Pop_Reference)
                 .Set_Size (New_Size)
                 .Done;
            end;
         end loop;
      end;

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
