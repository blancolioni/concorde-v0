with Ada.Containers.Doubly_Linked_Lists;

with Concorde.Colonies;

with Concorde.Logging;
with Concorde.Money;
with Concorde.Quantities;
with Concorde.Random;
with Concorde.Real_Images;

with Concorde.Handles.Colony;
with Concorde.Handles.Colony_Price;
with Concorde.Handles.Colony_District;
with Concorde.Handles.Commodity;
with Concorde.Handles.Deposit;
with Concorde.Handles.Pop;
with Concorde.Handles.Pop_Group;
with Concorde.Handles.Resource;
with Concorde.Handles.World_Sector;
with Concorde.Handles.District;

package body Concorde.Managers.Colonies is

   type Pop_Group_Values is
      record
         Group       : Concorde.Handles.Pop_Group.Pop_Group_Handle;
         Income      : Concorde.Handles.Network_Value.Network_Value_Handle;
         Tax_Rate    : Concorde.Handles.Network_Value.Network_Value_Handle;
         Tax_Evasion : Concorde.Handles.Network_Value.Network_Value_Handle;
         Happiness   : Concorde.Handles.Network_Value.Network_Value_Handle;
      end record;

   package Pop_Group_Value_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Pop_Group_Values);

   type Pop_Cache_Entry is
      record
         Node   : Concorde.Network.Network_Value_Type;
         Group  : Concorde.Handles.Pop_Group.Pop_Group_Handle;
      end record;

   package Pop_Cache_Maps is
     new WL.String_Maps (Pop_Cache_Entry);

   type Commodity_Cache_Entry is
      record
         Node   : Concorde.Network.Network_Value_Type;
         Ref    : Concorde.Handles.Commodity.Commodity_Handle;
      end record;

   package Commodity_Cache_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Commodity_Cache_Entry);

   type Root_Colony_Manager_Type is
     new Root_Manager_Type with
      record
         Colony      : Concorde.Handles.Colony.Colony_Handle;
         Group_Nodes : Pop_Group_Value_Lists.List;
         Network     : Concorde.Network.Network_Type;
         Pop_Cache   : Pop_Cache_Maps.Map;
         Commodities : Commodity_Cache_Lists.List;
      end record;

   overriding function Identifier
     (Manager : Root_Colony_Manager_Type)
      return String
   is ("default-colony-manager");

   overriding procedure Activate
     (Manager : not null access Root_Colony_Manager_Type);

   function Image (X : Real) return String
                   renames Concorde.Real_Images.Approximate_Image;

   --------------
   -- Activate --
   --------------

   overriding procedure Activate
     (Manager : not null access Root_Colony_Manager_Type)
   is
      Colony : constant Concorde.Handles.Colony.Colony_Handle :=
                 Manager.Colony;
      Network : constant Concorde.Network.Network_Type :=
                  Manager.Network;

      procedure Update_Colony_Prices;

      procedure Update_Population_Demands;

      procedure Update_Mining_Production;

      procedure Update_Population_Sizes;

      --------------------------
      -- Update_Colony_Prices --
      --------------------------

      procedure Update_Colony_Prices is
      begin
         for Item of Manager.Commodities loop
            declare
               Price : constant Real :=
                         Concorde.Money.To_Real
                           (Concorde.Handles.Colony_Price
                            .Get_By_Commodity_Price
                              (Manager.Colony, Item.Ref)
                            .Price);
            begin
               Concorde.Network.Update_Value (Item.Node, Price);
            end;
         end loop;
      end Update_Colony_Prices;

      ------------------------------
      -- Update_Mining_Production --
      ------------------------------

      procedure Update_Mining_Production is

         package Resource_Mining_Maps is
           new WL.String_Maps (Non_Negative_Real);

         Mined_Resource : Resource_Mining_Maps.Map;

         Mine : constant Concorde.Handles.District.District_Handle :=
                  Concorde.Handles.District.Get_By_Tag ("mine");
      begin

         for Colony_District of
           Concorde.Handles.Colony_District.Select_By_Colony_District
             (Colony => Manager.Colony,
              District   => Mine)
         loop
            declare
               Sector  : constant Concorde.Handles.World_Sector
                 .World_Sector_Class :=
                   Colony_District.World_Sector;
               Deposit : constant Concorde.Handles.Deposit.Deposit_Handle :=
                                       Concorde.Handles.Deposit
                                         .Get_By_World_Sector (Sector);
            begin
               if Deposit.Has_Element then
                  declare
                     Tag : constant String :=
                             Deposit.Resource.Tag;
                     Concentration : constant Real :=
                                       Deposit.Concentration;
                     Available     : constant Real :=
                                       Concorde.Quantities.To_Real
                                         (Deposit.Available);
                     Factor        : constant Real :=
                                       Real'Max
                                         (Concentration / 10.0,
                                          Concorde.Random.Normal_Random
                                            (Concentration / 10.0)
                                          + Concentration);
                     Mined         : constant Non_Negative_Real :=
                                       Factor * Colony_District.Size;
                     Remaining     : constant Non_Negative_Real :=
                                       Real'Max (Available - Mined / 100.0,
                                                 0.0);
                     New_Conc      : constant Real :=
                                       Concentration
                                         * (1.0 - Mined / Remaining / 100.0);
                  begin
                     if Mined > 0.0 then
                        if not Mined_Resource.Contains (Tag) then
                           Mined_Resource.Insert (Tag, Mined);
                        else
                           Mined_Resource (Tag) :=
                             Mined_Resource (Tag) + Mined;
                        end if;

                        Concorde.Logging.Log
                          (Actor    => "mine",
                           Location => Colony.World.Name,
                           Category => Tag,
                           Message  =>
                             "available "
                           & Image (Available)
                           & "; concentration "
                           & Image (Concentration * 100.0)
                           & "%"
                           & "; factor "
                           & Image (Factor * 100.0) & "%"
                           & "; mined "
                           & Image (Mined)
                           & " remaining "
                           & Image (Remaining)
                           & " new concentration "
                           & Image (New_Conc * 100.0) & "%");
                        Deposit.Update_Deposit
                          .Set_Concentration (New_Conc)
                          .Set_Difficulty (1.0 - New_Conc)
                          .Set_Available
                            (Concorde.Quantities.To_Quantity (Remaining))
                          .Done;
                     end if;
                  end;
               end if;
            end;
         end loop;

         for Position in Mined_Resource.Iterate loop
            declare
               Tag : constant String :=
                       Resource_Mining_Maps.Key (Position) & "-production";
               Quantity : constant Real :=
                            Resource_Mining_Maps.Element (Position);
            begin
               Concorde.Network.Update_Value
                 (Manager.Network, Tag, Quantity);
            end;
         end loop;

      end Update_Mining_Production;

      -------------------------------
      -- Update_Population_Demands --
      -------------------------------

      procedure Update_Population_Demands is

         package Real_Maps is
           new WL.String_Maps (Real);

         Remaining_Map : Real_Maps.Map;

         function Remaining
           (Commodity : Concorde.Handles.Commodity.Commodity_Class)
            return Non_Negative_Real;

         procedure Receive
           (Commodity : Concorde.Handles.Commodity.Commodity_Class;
            Quantity  : Real);

         -------------
         -- Receive --
         -------------

         procedure Receive
           (Commodity : Concorde.Handles.Commodity.Commodity_Class;
            Quantity  : Real)
         is
            Tag : constant String := Commodity.Tag;
         begin
            pragma Assert (Remaining_Map.Contains (Tag),
                           "remaining map did not contain " & Tag);
            Remaining_Map (Tag) := Remaining_Map (Tag) - Quantity;
         end Receive;

         ---------------
         -- Remaining --
         ---------------

         function Remaining
           (Commodity : Concorde.Handles.Commodity.Commodity_Class)
            return Non_Negative_Real
         is
            Tag : constant String := Commodity.Tag;
         begin
            if not Remaining_Map.Contains (Tag) then
               Remaining_Map.Insert
                 (Tag,
                  Concorde.Network.Current_Value
                    (Manager.Network, Tag & "-supply"));
            end if;

            declare
               R : constant Real := Remaining_Map.Element (Tag);
            begin
               if R < 0.0 then
                  Concorde.Logging.Log
                    (Commodity.Tag,
                     "remaining was negative: " & Image (R));
                  return 0.0;
               else
                  return R;
               end if;
            end;
         end Remaining;

      begin
         for Pop_Group of
           Concorde.Handles.Wealth_Group.Scan_By_Priority
         loop

            for Pop_Group_Demand of
              Concorde.Handles.Pop_Group_Demand.Select_By_Pop_Group
                (Pop_Group)
            loop
               declare
                  Commodity        : constant Concorde.Handles.Commodity
                    .Commodity_Class :=
                                 Pop_Group_Demand.Commodity;
                  Demand_Tag : constant String :=
                                 Pop_Group.Tag
                                 & "-"
                                 & Commodity.Tag
                                 & "-"
                                 & "demand";
                  Receive_Tag : constant String :=
                                  Pop_Group.Tag
                                  & "-"
                                  & Commodity.Tag
                                  & "-"
                                  & "recv";

                  Demand     : constant Real :=
                                  Concorde.Network.Current_Value
                                    (Network, Demand_Tag);
                  Supply     : constant Real :=
                                 Remaining (Commodity);
                  Received   : constant Real := Real'Min (Supply, Demand);
               begin
                  Concorde.Logging.Log
                    ("pop-demand",
                     "demand=" & Image (Demand)
                     & "; supply=" & Image (Supply)
                     & "; received=" & Image (Received));

                  Concorde.Network.Update_Value
                    (Network, Receive_Tag, Received);
                  Receive (Commodity, Received);
               end;
            end loop;
         end loop;
      end Update_Population_Demands;

      -----------------------------
      -- Update_Population_Sizes --
      -----------------------------

      procedure Update_Population_Sizes is

         package Pop_Group_Maps is
           new WL.String_Maps (Real);

         Group_Size : Pop_Group_Maps.Map;

         procedure Add_Pop
           (Group : Concorde.Handles.Pop_Group.Pop_Group_Class;
            Size  : Non_Negative_Real);

         -------------
         -- Add_Pop --
         -------------

         procedure Add_Pop
           (Group : Concorde.Handles.Pop_Group.Pop_Group_Class;
            Size  : Non_Negative_Real)
         is
            Tag : constant String := Group.Tag;
         begin
            if not Group_Size.Contains (Tag) then
               Group_Size.Insert (Tag, 0.0);
            end if;
            Group_Size (Tag) := Group_Size (Tag) + Size;
         end Add_Pop;

      begin

         for Pop_Group of Concorde.Handles.Pop_Group.Scan_By_Tag loop
            Group_Size.Insert (Pop_Group.Tag, 0.0);
         end loop;

         for Pop of Concorde.Handles.Pop.Select_By_Colony (Manager.Colony) loop
            for Group of
              Concorde.Handles.Pop_Group_Member.Select_By_Pop
                (Pop)
            loop
               Add_Pop (Group.Pop_Group, Pop.Size);
            end loop;
         end loop;

         for Cache_Entry of Manager.Pop_Cache loop
            declare
               Tag : constant String :=
                       Cache_Entry.Group.Tag;
               Size : constant Non_Negative_Real :=
                        Group_Size.Element (Tag);
            begin
               Concorde.Network.Update_Value
                 (Cache_Entry.Node, Size);

               Concorde.Logging.Log
                 (Actor    => Tag,
                  Location => Colony.World.Name,
                  Category => "size",
                  Message  => Image (Size));
            end;
         end loop;

      end Update_Population_Sizes;

   begin
      Concorde.Logging.Log
        (Actor    => Colony.World.Name,
         Location => "colony",
         Category => "managed",
         Message  => "activating");

      Update_Colony_Prices;

      Update_Population_Demands;

      Update_Population_Sizes;

      Update_Mining_Production;

      Concorde.Network.Update_Network (Manager.Network);

      for Policy of
        Concorde.Handles.Policy.Scan_By_Tag
      loop
         Concorde.Handles.Colony_Policy.Update_Colony_Policy
           (Concorde.Handles.Colony_Policy.Get_By_Colony_Policy
              (Manager.Colony, Policy))
           .Set_Revenue (Concorde.Money.Zero)
           .Set_Expense (Concorde.Money.Zero)
           .Done;
      end loop;

      for Pop of Concorde.Handles.Pop.Select_By_Colony (Manager.Colony) loop
         Concorde.Colonies.Daily_Tax_Revenue
           (Manager.Colony, Pop);
      end loop;

      for Policy of
        Concorde.Handles.Policy.Scan_By_Tag
      loop
         Concorde.Colonies.Execute_Daily_Policy
           (Colony => Manager.Colony,
            Policy => Policy);
      end loop;

      declare
         Birth_Rate : constant Unit_Real :=
                        Concorde.Network.Current_Value
                          (Network, "birth-rate");
         Death_Rate : constant Unit_Real :=
                        Concorde.Network.Current_Value
                          (Network, "death-rate");
      begin
         for Pop of
           Concorde.Handles.Pop.Select_By_Colony (Manager.Colony)
         loop
            declare
               Births : constant Non_Negative_Real :=
                          Pop.Size * (Birth_Rate / 360.0);
               Deaths : constant Non_Negative_Real :=
                          Pop.Size * (Death_Rate / 360.0);
               New_Size : constant Non_Negative_Real :=
                            Real'Max (Pop.Size + Births - Deaths, 0.0);
            begin
               Concorde.Handles.Pop.Update_Pop (Pop)
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
     (Managed : Concorde.Handles.Managed.Managed_Class) return Manager_Type
   is
      use Concorde.Calendar;
      Colony  : constant Concorde.Handles.Colony.Colony_Handle :=
                  Concorde.Handles.Colony.Get_From_Managed (Managed);
      Manager : Root_Colony_Manager_Type :=
                  Root_Colony_Manager_Type'
                    (Managed         => Managed_Holders.To_Holder (Managed),
                     Is_Active       => True,
                     Has_Next_Update => True,
                     Next_Update     =>
                       Concorde.Calendar.Clock
                     + Concorde.Calendar.Days (Concorde.Random.Unit_Random),
                     Colony          => Colony,
                     Network         =>
                       Concorde.Network.Get_Network (Colony),
                     Group_Nodes     => Pop_Group_Value_Lists.Empty_List,
                     Pop_Cache       => Pop_Cache_Maps.Empty_Map,
                     Commodities     => Commodity_Cache_Lists.Empty_List);

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
            return Concorde.Handles.Network_Value.Network_Value_Handle;

         ---------
         -- Get --
         ---------

         function Get
           (Suffix : String)
            return Concorde.Handles.Network_Value.Network_Value_Handle
         is
            Tag : constant String :=
                    Name & (if Suffix = "" then "" else "-" & Suffix);
            Node : constant Concorde.Handles.Node.Node_Handle :=
                     Concorde.Handles.Node.Get_By_Tag (Tag);
         begin
            pragma Assert (Node.Has_Element);
            return Concorde.Handles.Network_Value.Get_By_Network_Value
              (Colony, Node);
         end Get;

         Group : constant Pop_Group_Values := Pop_Group_Values'
           (Group       => Concorde.Handles.Pop_Group.Get_By_Tag (Name),
            Income      => Get ("income"),
            Tax_Rate    => Get ("income-tax-rate"),
            Tax_Evasion => Get ("tax-evasion"),
            Happiness   => Get (""));

         Value_Node    : constant Concorde.Network.Network_Value_Type :=
                           Concorde.Network.Get_Network_Value
                             (Manager.Network, Name & "-population");
      begin
         Manager.Group_Nodes.Append (Group);
         Manager.Pop_Cache.Insert
           (Name, Pop_Cache_Entry'
              (Node  => Value_Node,
               Group => Group.Group));
      end Add_Group;

   begin
      for Group of
        Concorde.Handles.Wealth_Group.Scan_By_Tag
      loop
         Add_Group (Group.Tag);
      end loop;

      for Commodity of Concorde.Handles.Commodity.Scan_By_Tag loop
         declare
            Tag : constant String := Commodity.Tag & "-base-price";
         begin
            Manager.Commodities.Append
              (Commodity_Cache_Entry'
                 (Node =>
                      Concorde.Network.Get_Network_Value
                        (Manager.Network, Tag),
                  Ref  =>
                    Concorde.Handles.Commodity.Commodity_Handle (Commodity)));
         end;
      end loop;

      return new Root_Colony_Manager_Type'(Manager);
   end Create_Default_Manager;

end Concorde.Managers.Colonies;
