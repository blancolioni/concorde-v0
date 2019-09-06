with Ada.Strings.Fixed;

with WL.Localisation;

with Concorde.Db.Commodity;
with Concorde.Db.Commodity_Class;
with Concorde.Db.Lease;
with Concorde.Db.Property_Entry;
with Concorde.Db.Stock_Item;

package body Concorde.Commodities is

   All_Commodities_List : Commodity_Lists.List;
   Skills_List          : Commodity_Lists.List;
   Title_Reference      : Concorde.Db.Commodity_Class_Reference :=
     Concorde.Db.Null_Commodity_Class_Reference;
   Lease_Reference      : Concorde.Db.Commodity_Class_Reference :=
     Concorde.Db.Null_Commodity_Class_Reference;

   function Get_Rec
     (Stock     : Stock_Type;
      Commodity : Concorde.Db.Commodity_Reference)
      return Stock_Record;

   function Lease_Tag
     (Commodity : Commodity_Reference;
      Days      : Positive)
      return String
   is ("L--"
       & Concorde.Db.Commodity.Get (Commodity).Tag
       & Integer'Image (-Days));

   ---------
   -- Add --
   ---------

   procedure Add
     (To    : in out Stock_Type;
      Other : Stock_Type)
   is
   begin
      for Rec of Other.List loop
         declare
            use Concorde.Money, Concorde.Quantities;
            To_Rec : constant Stock_Record :=
                       Get_Rec (To, Rec.Commodity);
         begin
            Set_Quantity (To, Rec.Commodity,
                          Rec.Quantity + To_Rec.Quantity,
                          Rec.Value + To_Rec.Value);
         end;
      end loop;
   end Add;

   ---------------------
   -- All_Commodities --
   ---------------------

   function All_Commodities return Commodity_Lists.List is
   begin
      if All_Commodities_List.Is_Empty then
         for Commodity of Concorde.Db.Commodity.Scan_By_Tag loop
            All_Commodities_List.Append (Commodity.Get_Commodity_Reference);
         end loop;
      end if;

      return All_Commodities_List;
   end All_Commodities;

   ------------
   -- Exists --
   ------------

   function Exists
     (Tag : String)
      return Boolean
   is
      use type Concorde.Db.Commodity_Reference;
   begin
      return Concorde.Db.Commodity.Get_Reference_By_Tag (Tag)
        /= Concorde.Db.Null_Commodity_Reference;
   end Exists;

   ---------
   -- Get --
   ---------

   function Get
     (Tag : String)
      return Commodity_Reference
   is
   begin
      return Concorde.Db.Commodity.Get_Reference_By_Tag (Tag);
   end Get;

   ------------------------
   -- Get_Price_Per_Item --
   ------------------------

   function Get_Price_Per_Item
     (Stock     : Stock_Type;
      Commodity : Concorde.Db.Commodity_Reference)
      return Concorde.Money.Price_Type
   is
      use Concorde.Quantities;
      Rec : constant Stock_Record := Stock.Get_Rec (Commodity);
   begin
      if Rec.Quantity = Zero then
         return Concorde.Money.Zero;
      else
         return Concorde.Money.Price (Rec.Value, Rec.Quantity);
      end if;
   end Get_Price_Per_Item;

   ------------------
   -- Get_Property --
   ------------------

   function Get_Property
     (Commodity : Commodity_Reference;
      Property  : Concorde.Properties.Property_Type)
      return Real
   is
   begin
      return Concorde.Db.Property_Entry.Get_By_Property_Entry
        (Has_Properties => Concorde.Db.Commodity.Get (Commodity)
         .Get_Has_Properties_Reference,
         Property       => Property)
        .Value;
   end Get_Property;

   ------------------
   -- Get_Quantity --
   ------------------

   function Get_Quantity
     (Stock     : Stock_Type;
      Commodity : Concorde.Db.Commodity_Reference)
      return Concorde.Quantities.Quantity_Type
   is
      use type Concorde.Db.Commodity_Reference;
   begin
      for Rec of Stock.List loop
         if Rec.Commodity = Commodity then
            return Rec.Quantity;
         end if;
      end loop;
      return Concorde.Quantities.Zero;
   end Get_Quantity;

   -------------
   -- Get_Rec --
   -------------

   function Get_Rec
     (Stock     : Stock_Type;
      Commodity : Concorde.Db.Commodity_Reference)
      return Stock_Record
   is
      use type Concorde.Db.Commodity_Reference;
   begin
      for Rec of Stock.List loop
         if Rec.Commodity = Commodity then
            return Rec;
         end if;
      end loop;
      return (Commodity, Concorde.Quantities.Zero, Concorde.Money.Zero);
   end Get_Rec;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value
     (Stock     : Stock_Type;
      Commodity : Concorde.Db.Commodity_Reference)
      return Concorde.Money.Money_Type
   is
      use type Concorde.Db.Commodity_Reference;
   begin
      for Rec of Stock.List loop
         if Rec.Commodity = Commodity then
            return Rec.Value;
         end if;
      end loop;
      return Concorde.Money.Zero;
   end Get_Value;

   ------------------
   -- Has_Property --
   ------------------

   function Has_Property
     (Commodity : Commodity_Reference;
      Property  : Concorde.Properties.Property_Type)
      return Boolean
   is
   begin
      return Concorde.Db.Property_Entry.Is_Property_Entry
        (Has_Properties => Concorde.Db.Commodity.Get (Commodity)
         .Get_Has_Properties_Reference,
         Property       => Property);
   end Has_Property;

   -------------------
   -- Initial_Price --
   -------------------

   function Initial_Price
     (Commodity : Commodity_Reference)
      return Concorde.Money.Price_Type
   is
   begin
      return Concorde.Db.Commodity.Get (Commodity).Initial_Price;
   end Initial_Price;

   --------------
   -- Is_Lease --
   --------------

   function Is_Lease
     (Commodity : Commodity_Reference)
      return Boolean
   is
      use Concorde.Db;
   begin
      return Concorde.Db.Commodity.Get (Commodity).Top_Record
        = Concorde.Db.R_Lease;
   end Is_Lease;

   --------------
   -- Is_Skill --
   --------------

   function Is_Skill (Commodity : Commodity_Reference) return Boolean is
   begin
      return Skills_List.Contains (Commodity);
   end Is_Skill;

   --------------
   -- Is_Title --
   --------------

   function Is_Title
     (Commodity : Commodity_Reference)
      return Boolean
   is
      use Concorde.Db;
   begin
      return Concorde.Db.Commodity.Get (Commodity).Commodity_Class
        = Title_Category;
   end Is_Title;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Stock   : Stock_Type;
      Process : not null access
        procedure (Commodity : Concorde.Db.Commodity_Reference;
                   Quantity  : Concorde.Quantities.Quantity_Type;
                   Value     : Concorde.Money.Money_Type))
   is
      use Concorde.Quantities;
   begin
      for Rec of Stock.List loop
         if Rec.Quantity > Zero then
            Process (Rec.Commodity, Rec.Quantity, Rec.Value);
         end if;
      end loop;
   end Iterate;

   function Lease
     (Commodity : Commodity_Reference;
      Days      : Positive)
      return Commodity_Reference
   is
      Tag : constant String := Lease_Tag (Commodity, Days);
   begin
      if not Exists (Tag) then
         Concorde.Db.Lease.Create
           (Commodity_Class  =>
              Concorde.Db.Commodity_Class.Get_Reference_By_Tag ("lease"),
            Initial_Price    =>
              Concorde.Money.Adjust_Price
                (Initial_Price (Commodity),
                 Non_Negative_Real (Days) / (10.0 * 360.0)),
            Mass             => 0.1,
            Density          => 0.1,
            Tag              => Tag,
            Leased_Commodity => Commodity,
            Term             => Days);
      end if;

      return Get (Tag);
   end Lease;

   --------------------
   -- Lease_Category --
   --------------------

   function Lease_Category
     return Concorde.Db.Commodity_Class_Reference
   is
      use Concorde.Db;
   begin
      if Lease_Reference = Null_Commodity_Class_Reference then
         Lease_Reference :=
           Concorde.Db.Commodity_Class.Get_Reference_By_Tag ("lease");
      end if;
      return Lease_Reference;
   end Lease_Category;

   -----------------------
   -- Lease_Commodities --
   -----------------------

   function Lease_Commodities
     (For_Commodity : Commodity_Reference)
      return Commodity_Lists.List
   is
   begin
      return List : Commodity_Lists.List do
         for Lease of
           Concorde.Db.Lease.Select_By_Leased_Commodity (For_Commodity)
         loop
            List.Append (Lease.Get_Commodity_Reference);
         end loop;
      end return;
   end Lease_Commodities;

   ----------------
   -- Lease_Days --
   ----------------

   function Lease_Days
     (Commodity : Commodity_Reference)
      return Positive
   is
   begin
      return Concorde.Db.Lease.Get_Lease (Commodity).Term;
   end Lease_Days;

   ----------------------
   -- Leased_Commodity --
   ----------------------

   function Leased_Commodity
     (Commodity : Commodity_Reference)
      return Commodity_Reference
   is
   begin
      return Concorde.Db.Lease.Get_Lease (Commodity).Leased_Commodity;
   end Leased_Commodity;

   ----------
   -- Load --
   ----------

   procedure Load
     (Stock     : in out Stock_Type;
      Has_Stock : Concorde.Db.Has_Stock_Reference)
   is
      use Concorde.Quantities;
   begin
      for Item of Concorde.Db.Stock_Item.Select_By_Has_Stock (Has_Stock) loop
         if Item.Quantity > Zero then
            Stock.Set_Quantity
              (Item.Commodity, Item.Quantity, Item.Value);
         end if;
      end loop;
   end Load;

   ----------------
   -- Local_Name --
   ----------------

   function Local_Name
     (Commodity : Commodity_Reference)
      return String
   is
      Tag : constant String :=
              Concorde.Db.Commodity.Get (Commodity).Tag;
   begin
      if WL.Localisation.Has_Local_Text (Tag) then
         return WL.Localisation.Local_Text (Tag);
      else
         return Tag;
      end if;
   end Local_Name;

   -------------
   -- Missing --
   -------------

   function Missing
     (Available_Stock : Stock_Type;
      Required_Stock  : Stock_Type)
      return Stock_Type
   is
   begin
      return Stock : Stock_Type do
         for Rec of Required_Stock.List loop
            declare
               use Concorde.Money, Concorde.Quantities;
               Available : constant Stock_Record :=
                             Get_Rec (Available_Stock, Rec.Commodity);
               Missing   : constant Quantity_Type :=
                             (if Available.Quantity < Rec.Quantity
                              then Rec.Quantity - Available.Quantity
                              else Zero);
               Value     : constant Concorde.Money.Money_Type :=
                             Concorde.Money.Total
                               (Concorde.Money.Price
                                  (Available.Value + Rec.Value,
                                   Available.Quantity + Rec.Quantity),
                                Missing);
            begin
               if Missing > Zero and then Value > Zero then
                  Stock.Set_Quantity
                    (Rec.Commodity, Missing, Value);
               end if;
            end;
         end loop;
      end return;
   end Missing;

   ------------------
   -- Set_Quantity --
   ------------------

   procedure Set_Quantity
     (Stock     : in out Stock_Type;
      Commodity : Concorde.Db.Commodity_Reference;
      Quantity  : Concorde.Quantities.Quantity_Type;
      Value     : Concorde.Money.Money_Type)
   is
      use Concorde.Money, Concorde.Quantities;
      use type Concorde.Db.Commodity_Reference;
   begin
      pragma Assert (Value > Zero or else Quantity = Zero);
      Stock.Total_Quantity := Stock.Total_Quantity + Quantity;
      Stock.Total_Value := Stock.Total_Value + Value;

      for Rec of Stock.List loop
         if Rec.Commodity = Commodity then
            Stock.Total_Quantity := Stock.Total_Quantity - Rec.Quantity;
            Stock.Total_Value := Stock.Total_Value - Rec.Value;
            Rec.Quantity := Quantity;
            Rec.Value    := Value;
            return;
         end if;
      end loop;
      Stock.List.Append ((Commodity, Quantity, Value));
   end Set_Quantity;

   ------------------
   -- Set_Quantity --
   ------------------

   procedure Set_Quantity
     (Stock     : in out Stock_Type;
      Commodity : Concorde.Db.Commodity_Reference;
      Quantity  : Concorde.Quantities.Quantity_Type;
      Price_Per : Concorde.Money.Price_Type)
   is
   begin
      Stock.Set_Quantity (Commodity, Quantity,
                          Concorde.Money.Total (Price_Per, Quantity));
   end Set_Quantity;

   ------------
   -- Skills --
   ------------

   function Skills return Commodity_Lists.List is
   begin
      if Skills_List.Is_Empty then
         declare
            Skill_Class : constant Concorde.Db.Commodity_Class_Reference :=
              Concorde.Db.Commodity_Class.Get_Reference_By_Tag
                ("skills");
         begin
            for Commodity of
              Concorde.Db.Commodity.Select_By_Commodity_Class
                (Skill_Class)
            loop
               Skills_List.Append (Commodity.Get_Commodity_Reference);
            end loop;
         end;
      end if;

      return Skills_List;
   end Skills;

   --------------------
   -- Title_Category --
   --------------------

   function Title_Category
     return Concorde.Db.Commodity_Class_Reference
   is
      use Concorde.Db;
   begin
      if Title_Reference = Null_Commodity_Class_Reference then
         Title_Reference :=
           Concorde.Db.Commodity_Class.Get_Reference_By_Tag ("title");
      end if;
      return Title_Reference;
   end Title_Category;

   ---------------------
   -- Title_Commodity --
   ---------------------

   function Title_Commodity
     (Sector     : Concorde.Db.World_Sector_Reference;
      Title_Zone : Commodity_Reference)
      return Commodity_Reference
   is
   begin
      return Concorde.Db.Commodity.Get_Reference_By_Tag
        (Title_Tag (Sector, Title_Zone));
   end Title_Commodity;

   ---------------
   -- Title_Tag --
   ---------------

   function Title_Tag
     (Sector     : Concorde.Db.World_Sector_Reference;
      Title_Type : Commodity_Reference)
      return String
   is
   begin
      return "T--"
        & Concorde.Db.Commodity.Get (Title_Type).Tag
        & "-"
        & Ada.Strings.Fixed.Trim
        (Concorde.Db.To_String (Sector), Ada.Strings.Both);
   end Title_Tag;

   --------------------
   -- Total_Quantity --
   --------------------

   function Total_Quantity
     (Stock     : Stock_Type)
      return Concorde.Quantities.Quantity_Type
   is
   begin
      return Stock.Total_Quantity;
   end Total_Quantity;

   -----------------
   -- Total_Value --
   -----------------

   function Total_Value
     (Stock     : Stock_Type)
      return Concorde.Money.Money_Type
   is
   begin
      return Stock.Total_Value;
   end Total_Value;

end Concorde.Commodities;
