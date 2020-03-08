with Tropos.Reader;

with Concorde.Commodities;

with Concorde.Db.Commodity_Group;
with Concorde.Db.Production;
with Concorde.Db.Input_Item;
with Concorde.Db.Output_Item;
with Concorde.Db.Efficiency_Item;
with Concorde.Db.Required_Item;
with Concorde.Db.Zone;

package body Concorde.Configure.Production is

   procedure Configure_Production
     (Production_Config : Tropos.Configuration);

   procedure Configure_Production_Items
     (Production : Concorde.Db.Production_Reference;
      Config     : Tropos.Configuration;
      Create : not null access
        procedure (Production : Concorde.Db.Production_Reference;
                   Commodity  : Concorde.Db.Commodity_Reference;
                   Category   : Concorde.Db.Commodity_Group_Reference;
                   Quantity   : Concorde.Quantities.Quantity_Type));

   --------------------------
   -- Configure_Production --
   --------------------------

   procedure Configure_Production
     (Scenario_Name : String)
   is
   begin
      for Production_Config of
        Tropos.Reader.Read_Config
          (Scenario_Directory (Scenario_Name, "production"),
           "production")
      loop
         Configure_Production (Production_Config);
      end loop;
   end Configure_Production;

   --------------------------
   -- Configure_Production --
   --------------------------

   procedure Configure_Production
     (Production_Config : Tropos.Configuration)
   is
      use type Concorde.Db.Zone_Reference;
      Zone : constant Concorde.Db.Zone_Reference :=
        Concorde.Db.Zone.Get_Reference_By_Tag
          (Production_Config.Get ("zone", ""));
      Size : constant Non_Negative_Real :=
        Real (Float'(Production_Config.Get ("size", 1.0)));
      P : constant Concorde.Db.Production_Reference :=
        Concorde.Db.Production.Create
          (Tag  => Production_Config.Config_Name,
           Zone => Zone,
           Size => Size);
   begin

      if Zone = Concorde.Db.Null_Zone_Reference then
         raise Constraint_Error with
           "in production " & Production_Config.Config_Name
           & ": no such zone: "
           & Production_Config.Get ("zone");
      end if;

      Configure_Production_Items
        (Production => P,
         Config     => Production_Config.Child ("in"),
         Create     => Concorde.Db.Input_Item.Create'Access);
      Configure_Production_Items
        (Production => P,
         Config     => Production_Config.Child ("out"),
         Create     => Concorde.Db.Output_Item.Create'Access);
      Configure_Production_Items
        (Production => P,
         Config     => Production_Config.Child ("efficiency"),
         Create     => Concorde.Db.Efficiency_Item.Create'Access);
      Configure_Production_Items
        (Production => P,
         Config     => Production_Config.Child ("with"),
         Create     => Concorde.Db.Required_Item.Create'Access);
   end Configure_Production;

   --------------------------------
   -- Configure_Production_Items --
   --------------------------------

   procedure Configure_Production_Items
     (Production : Concorde.Db.Production_Reference;
      Config     : Tropos.Configuration;
      Create     : not null access
        procedure (Production : Concorde.Db.Production_Reference;
                   Commodity  : Concorde.Db.Commodity_Reference;
                   Category   : Concorde.Db.Commodity_Group_Reference;
                   Quantity   : Concorde.Quantities.Quantity_Type))
   is
   begin
      for Item of Config loop
         if Concorde.Commodities.Exists (Item.Config_Name) then
            Create (Production,
                    Concorde.Commodities.To_Database_Reference
                      (Concorde.Commodities.Get (Item.Config_Name)),
                    Concorde.Db.Null_Commodity_Group_Reference,
                    Concorde.Quantities.To_Quantity
                      (Real (Float'(Item.Value))));
         else
            declare
               use type Concorde.Db.Commodity_Group_Reference;
               Class : constant Concorde.Db.Commodity_Group_Reference :=
                 Concorde.Db.Commodity_Group.Get_Reference_By_Tag
                   (Item.Config_Name);
            begin
               if Class /= Concorde.Db.Null_Commodity_Group_Reference then
                  Create (Production, Concorde.Db.Null_Commodity_Reference,
                          Class,
                          Concorde.Quantities.To_Quantity
                            (Real (Float'(Item.Value))));
               else
                  raise Constraint_Error with
                    "no such commodity or class: " & Item.Config_Name;
               end if;
            end;
         end if;
      end loop;
   end Configure_Production_Items;

end Concorde.Configure.Production;
