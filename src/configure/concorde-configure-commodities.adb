with Ada.Text_IO;

with Tropos.Reader;

with Concorde.Commodities;
with Concorde.Properties;
with Concorde.Money;

with Concorde.Db.Commodity_Class;
with Concorde.Db.Commodity;
with Concorde.Db.Construction_Input;
with Concorde.Db.Property_Entry;
with Concorde.Db.Resource;
with Concorde.Db.Skill;
with Concorde.Db.Stock_Item;
with Concorde.Db.Supply_Input;

package body Concorde.Configure.Commodities is

   Next_Index : Natural := 0;

   Resource_Category : Concorde.Db.Commodity_Class_Reference :=
     Concorde.Db.Null_Commodity_Class_Reference;
   Skill_Category    : Concorde.Db.Commodity_Class_Reference :=
     Concorde.Db.Null_Commodity_Class_Reference;

   procedure Configure_Category
     (Category_Config : Tropos.Configuration);

   procedure Configure_Commodity
     (Category : Concorde.Db.Commodity_Class_Reference;
      Config   : Tropos.Configuration);

   ------------------------
   -- Configure_Category --
   ------------------------

   procedure Configure_Category
     (Category_Config : Tropos.Configuration)
   is
      Name : constant String := Category_Config.Config_Name;
      Category : constant Concorde.Db.Commodity_Class_Reference :=
        Concorde.Db.Commodity_Class.Create (Name);
   begin
      if Name = "skill" then
         Skill_Category := Category;
      elsif Name = "resource" then
         Resource_Category := Category;
      end if;

      for Commodity_Config of Category_Config loop
         Configure_Commodity
           (Category, Commodity_Config);
      end loop;
   end Configure_Category;

   ---------------------------
   -- Configure_Commodities --
   ---------------------------

   procedure Configure_Commodities (Scenario_Name : String) is
   begin
      for Category_Config of
        Tropos.Reader.Read_Config
          (Scenario_Directory
             (Scenario_Name, "commodities"),
           "commodity")
      loop
         Configure_Category (Category_Config);
      end loop;
   end Configure_Commodities;

   -------------------------
   -- Configure_Commodity --
   -------------------------

   procedure Configure_Commodity
     (Category : Concorde.Db.Commodity_Class_Reference;
      Config   : Tropos.Configuration)
   is
      use type Concorde.Db.Commodity_Class_Reference;

      function Create_Commodity
        (Tag           : String;
         Initial_Price : Concorde.Money.Price_Type)
         return Concorde.Db.Commodity_Reference;

      ----------------------
      -- Create_Commodity --
      ----------------------

      function Create_Commodity
        (Tag           : String;
         Initial_Price : Concorde.Money.Price_Type)
         return Concorde.Db.Commodity_Reference
      is
      begin

         Next_Index := Next_Index + 1;

         if Category = Skill_Category then
            declare
               Ref : constant Concorde.Db.Skill_Reference :=
                 Concorde.Db.Skill.Create
                   (Category, Next_Index, Initial_Price, 1.0, 1.0, Tag);
            begin
               return Concorde.Db.Skill.Get (Ref).Get_Commodity_Reference;
            end;
         elsif Category = Resource_Category then
            declare
               Ref : constant Concorde.Db.Resource_Reference :=
                 Concorde.Db.Resource.Create
                   (Category, Next_Index, Initial_Price, 1.0, 1.0, Tag);
            begin
               return Concorde.Db.Resource.Get (Ref).Get_Commodity_Reference;
            end;
         else
            return Concorde.Db.Commodity.Create
              (Tag, Category, Next_Index, Initial_Price, 1.0, 1.0);
         end if;
      end Create_Commodity;

      Commodity : constant Concorde.Db.Commodity_Reference :=
        Create_Commodity
          (Tag           => Config.Config_Name,
           Initial_Price =>
             Concorde.Money.To_Price
               (Real (Float'(Config.Get ("base-price", 1.0)))));
      Has_Properties : constant Concorde.Db.Has_Properties_Reference :=
        Concorde.Db.Commodity.Get (Commodity).Get_Has_Properties_Reference;

   begin

      for Property_Config of Config loop
         if Property_Config.Config_Name = "education" then
            null;
         elsif Property_Config.Child_Count > 1 then
            begin
               for I in 1 .. Property_Config.Child_Count loop
                  Concorde.Db.Property_Entry.Create
                    (Has_Properties => Has_Properties,
                     Property       =>
                       Concorde.Properties.Get_Reference
                         (Property_Config.Config_Name
                          & Integer'Image (-I)),
                     Value          =>
                       Real (Float'(Property_Config.Get (I))));
               end loop;
            exception
               when Constraint_Error =>
                  Ada.Text_IO.Put_Line
                    (Ada.Text_IO.Standard_Error,
                     "property '" & Property_Config.Config_Name & "'"
                     & " in commodity '" & Config.Config_Name & "'"
                     & ": bad value: '" & Property_Config.Value);
            end;
         else
            begin
               Concorde.Db.Property_Entry.Create
                 (Has_Properties => Has_Properties,
                  Property       =>
                    Concorde.Properties.Get_Reference
                      (Property_Config.Config_Name),
                  Value          =>
                    Real (Float'(Property_Config.Value)));
            exception
               when Constraint_Error =>
                  Ada.Text_IO.Put_Line
                    (Ada.Text_IO.Standard_Error,
                       "property '" & Property_Config.Config_Name & "'"
                     & " in commodity '" & Config.Config_Name & "'"
                     & ": bad value: '" & Property_Config.Value);
            end;
         end if;
      end loop;
   end Configure_Commodity;

   ---------------------------
   -- Configure_Constructed --
   ---------------------------

   procedure Configure_Constructed
     (Constructed : Concorde.Db.Constructed_Reference;
      Config      : Tropos.Configuration;
      Factor      : Non_Negative_Real := 1.0)
   is
      Stock_Config : constant Tropos.Configuration :=
        (if Config.Contains ("build")
         then Config.Child ("build")
         else Config);
   begin
      for Item_Config of Stock_Config loop
         if Concorde.Commodities.Exists (Item_Config.Config_Name) then
            declare
               Commodity : constant Concorde.Commodities.Commodity_Reference :=
                 Concorde.Commodities.Get (Item_Config.Config_Name);
               Quantity  : constant Concorde.Quantities.Quantity_Type :=
                 Concorde.Quantities.Scale
                   (Concorde.Quantities.To_Quantity
                      (Real (Float'(Item_Config.Value))),
                    Factor);
            begin
               Concorde.Db.Construction_Input.Create
                 (Constructed => Constructed,
                  Commodity   =>
                    Concorde.Commodities.To_Database_Reference (Commodity),
                  Quantity    => Quantity);
            end;
         else
            raise Constraint_Error with
              "no such commodity in stock configuration: "
              & Item_Config.Config_Name;
         end if;
      end loop;
   end Configure_Constructed;

   ---------------------
   -- Configure_Stock --
   ---------------------

   procedure Configure_Stock
     (Has_Stock : Concorde.Db.Has_Stock.Has_Stock_Type;
      Config    : Tropos.Configuration;
      Factor    : Non_Negative_Real := 1.0)
   is
   begin
      Configure_Stock (Has_Stock.Get_Has_Stock_Reference, Config, Factor);
   end Configure_Stock;

   ---------------------
   -- Configure_Stock --
   ---------------------

   procedure Configure_Stock
     (Has_Stock : Concorde.Db.Has_Stock_Reference;
      Config    : Tropos.Configuration;
      Factor    : Non_Negative_Real := 1.0)
   is
      Stock_Config : constant Tropos.Configuration :=
                       (if Config.Contains ("stock")
                        then Config.Child ("stock")
                        else Config);
   begin
      for Item_Config of Stock_Config loop
         if Concorde.Commodities.Exists (Item_Config.Config_Name) then
            declare
               Commodity : constant Concorde.Commodities.Commodity_Reference :=
                 Concorde.Commodities.Get (Item_Config.Config_Name);
               Quantity  : constant Concorde.Quantities.Quantity_Type :=
                 Concorde.Quantities.Scale
                   (Concorde.Quantities.To_Quantity
                      (Real (Float'(Item_Config.Value))),
                    Factor);
               Value     : constant Concorde.Money.Money_Type :=
                 Concorde.Money.Total
                   (Concorde.Commodities.Initial_Price (Commodity),
                    Quantity);
            begin
               Concorde.Db.Stock_Item.Create
                 (Has_Stock => Has_Stock,
                  Commodity =>
                    Concorde.Commodities.To_Database_Reference (Commodity),
                  Quantity  => Quantity,
                  Value     => Value);
            end;
         else
            raise Constraint_Error with
              "no such commodity in stock configuration: "
              & Item_Config.Config_Name;
         end if;
      end loop;
   end Configure_Stock;

   ------------------------
   -- Configure_Supplied --
   ------------------------

   procedure Configure_Supplied
     (Supplied : Concorde.Db.Supplied_Reference;
      Config    : Tropos.Configuration;
      Factor    : Non_Negative_Real := 1.0)
   is
      Stock_Config : constant Tropos.Configuration :=
        (if Config.Contains ("supply")
         then Config.Child ("supply")
         else Config);
   begin
      for Item_Config of Stock_Config loop
         if Concorde.Commodities.Exists (Item_Config.Config_Name) then
            declare
               Commodity : constant Concorde.Commodities.Commodity_Reference :=
                 Concorde.Commodities.Get (Item_Config.Config_Name);
               Quantity  : constant Concorde.Quantities.Quantity_Type :=
                 Concorde.Quantities.Scale
                   (Concorde.Quantities.To_Quantity
                      (Real (Float'(Item_Config.Value))),
                    Factor);
            begin
               Concorde.Db.Supply_Input.Create
                 (Supplied => Supplied,
                  Commodity   =>
                    Concorde.Commodities.To_Database_Reference (Commodity),
                  Quantity    => Quantity);
            end;
         else
            raise Constraint_Error with
              "no such commodity in stock configuration: "
              & Item_Config.Config_Name;
         end if;
      end loop;
   end Configure_Supplied;

   --------------------------
   -- Next_Commodity_Index --
   --------------------------

   function Next_Commodity_Index return Positive is
   begin
      Next_Index := Next_Index + 1;
      return Next_Index;
   end Next_Commodity_Index;

end Concorde.Configure.Commodities;
