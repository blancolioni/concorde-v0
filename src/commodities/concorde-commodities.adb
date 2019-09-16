with Ada.Containers.Vectors;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

with Concorde.Db.Commodity;
with Concorde.Db.Commodity_Class;
with Concorde.Db.Lease;
with Concorde.Db.Property_Entry;
with Concorde.Db.Sector_Title;
with Concorde.Db.Stock_Item;
with Concorde.Db.Zone;

package body Concorde.Commodities is

   package Commodity_Subset_Vectors is
     new Ada.Containers.Vectors (Positive, Commodity_Reference);

   All_Commodities_Vector : Commodity_Subset_Vectors.Vector;
   Skills_Vector          : Commodity_Subset_Vectors.Vector;
   Leases_Vector          : Commodity_Subset_Vectors.Vector;

   Title_Reference      : Concorde.Db.Commodity_Class_Reference :=
     Concorde.Db.Null_Commodity_Class_Reference;
   Lease_Reference      : Concorde.Db.Commodity_Class_Reference :=
     Concorde.Db.Null_Commodity_Class_Reference;

   function Get_Rec
     (Stock     : Stock_Type;
      Commodity : Commodity_Reference)
      return Stock_Record;

   type Commodity_Record is
      record
         Reference              : Concorde.Db.Commodity_Reference;
         Lease_Reference        : Concorde.Db.Lease_Reference;
         Zone_Reference         : Concorde.Db.Zone_Reference;
         Sector_Title_Reference : Concorde.Db.Sector_Title_Reference;
         Sector_Reference       : Concorde.Db.World_Sector_Reference;
         Sector_Zone_Reference  : Commodity_Reference;
         Has_Properties         : Concorde.Db.Has_Properties_Reference;
         Tag                    : Ada.Strings.Unbounded.Unbounded_String;
         Local_Name             : Ada.Strings.Unbounded.Unbounded_String;
         Initial_Price          : Concorde.Money.Price_Type;
         Leased_Commodity       : Commodity_Reference;
         Lease_Days             : Natural;
         Is_Lease               : Boolean;
         Is_Skill               : Boolean;
         Is_Title               : Boolean;
         Is_Zone                : Boolean;
         Is_Sector_Title        : Boolean;
      end record;

   package Commodity_Vectors is
     new Ada.Containers.Vectors (Commodity_Reference, Commodity_Record);

   Vector : Commodity_Vectors.Vector;

   function Tag
     (Commodity : Commodity_Reference)
      return String
   is (Ada.Strings.Unbounded.To_String (Vector (Commodity).Tag));

   function Lease_Tag
     (Commodity : Commodity_Reference;
      Days      : Positive)
      return String
   is ("L--"
       & Tag (Commodity)
       & Integer'Image (-Days));

   procedure Check_Cache;

   procedure Add_To_Cache (Item : Concorde.Db.Commodity.Commodity_Type);

   function To_Array (Vector : Commodity_Subset_Vectors.Vector)
                      return Commodity_Array;

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

   ------------------
   -- Add_To_Cache --
   ------------------

   procedure Add_To_Cache (Item : Concorde.Db.Commodity.Commodity_Type) is

      use Concorde.Db;

      Ref : constant Concorde.Db.Commodity_Reference :=
        Item.Get_Commodity_Reference;

      function "+" (Item : String)
                    return Ada.Strings.Unbounded.Unbounded_String
                    renames Ada.Strings.Unbounded.To_Unbounded_String;

      Rec : Commodity_Record := Commodity_Record'
        (Reference              => Ref,
         Lease_Reference        => Concorde.Db.Null_Lease_Reference,
         Zone_Reference         => Concorde.Db.Null_Zone_Reference,
         Sector_Title_Reference => Concorde.Db.Null_Sector_Title_Reference,
         Sector_Reference       => Concorde.Db.Null_World_Sector_Reference,
         Sector_Zone_Reference  => 1,
         Has_Properties         => Item.Get_Has_Properties_Reference,
         Tag                    => +Item.Tag,
         Local_Name             => +Item.Tag,
         Initial_Price          => Item.Initial_Price,
         Leased_Commodity       => 1,
         Lease_Days             => 0,
         Is_Lease               => Item.Top_Record = R_Lease,
         Is_Skill               => Item.Top_Record = R_Skill,
         Is_Title               => Item.Top_Record = R_Sector_Title,
         Is_Zone                => Item.Top_Record = R_Zone,
         Is_Sector_Title        => Item.Top_Record = R_Sector_Title);
   begin

      if Rec.Is_Lease then
         declare
            Lease : constant Concorde.Db.Lease.Lease_Type :=
              Concorde.Db.Lease.Get_Lease (Item.Get_Commodity_Reference);
         begin
            Rec.Lease_Reference := Lease.Get_Lease_Reference;
            Rec.Leased_Commodity := Get_Commodity (Lease.Leased_Commodity);
            Rec.Lease_Days := Lease.Term;
         end;
      end if;

      if Rec.Is_Sector_Title then
         declare
            Title : constant Concorde.Db.Sector_Title.Sector_Title_Type :=
              Concorde.Db.Sector_Title.Get_Sector_Title (Ref);
         begin
            Rec.Sector_Title_Reference := Title.Get_Sector_Title_Reference;
            Rec.Sector_Zone_Reference := Get_Commodity (Title.Zone);
            Rec.Sector_Reference := Title.World_Sector;
         end;
      end if;

      if Rec.Is_Zone then
         Rec.Zone_Reference :=
           Concorde.Db.Zone.Get_Zone (Ref)
           .Get_Zone_Reference;
      end if;

      Vector.Append (Rec);

      if Rec.Is_Lease then
         Leases_Vector.Append (Vector.Last_Index);
      end if;

      if Rec.Is_Skill then
         Skills_Vector.Append (Vector.Last_Index);
      end if;

   end Add_To_Cache;

   ---------------------
   -- All_Commodities --
   ---------------------

   function All_Commodities return Commodity_Array is
   begin
      Check_Cache;
      return To_Array (All_Commodities_Vector);
   end All_Commodities;

   -----------------
   -- Check_Cache --
   -----------------

   procedure Check_Cache is
   begin
      if Vector.Is_Empty then
         for Item of Concorde.Db.Commodity.Scan_By_Index loop
            Add_To_Cache (Item);
         end loop;
      end if;

      if All_Commodities_Vector.Is_Empty then
         for Commodity in 1 .. Vector.Last_Index loop
            All_Commodities_Vector.Append (Commodity);
         end loop;
      end if;

      if Skills_Vector.Is_Empty then
         for Commodity in 1 .. Vector.Last_Index loop
            if Vector.Element (Commodity).Is_Skill then
               Skills_Vector.Append (Commodity);
            end if;
         end loop;
      end if;

      if Leases_Vector.Is_Empty then
         for Commodity in 1 .. Vector.Last_Index loop
            if Vector.Element (Commodity).Is_Skill then
               Skills_Vector.Append (Commodity);
            end if;
         end loop;
      end if;

   end Check_Cache;

   ------------------
   -- Create_Lease --
   ------------------

   function Create_Lease
     (Commodity : Commodity_Reference;
      Days      : Positive)
      return Commodity_Reference
   is
      Tag : constant String := Lease_Tag (Commodity, Days);
   begin
      if not Exists (Tag) then
         declare
            Lease : constant Concorde.Db.Lease_Reference :=
              Concorde.Db.Lease.Create
                (Commodity_Class  =>
                   Concorde.Db.Commodity_Class.Get_Reference_By_Tag ("lease"),
                 Index            =>
                   Natural (Vector.Last_Index) + 1,
                 Initial_Price    =>
                   Concorde.Money.Adjust_Price
                     (Initial_Price (Commodity),
                      Non_Negative_Real (Days) / (10.0 * 360.0)),
                 Mass             => 0.1,
                 Density          => 0.1,
                 Tag              => Tag,
                 Leased_Commodity => To_Database_Reference (Commodity),
                 Term             => Days);
         begin
            Add_To_Cache (Concorde.Db.Lease.Get (Lease));
         end;

      end if;

      return Get (Tag);
   end Create_Lease;

   ------------------
   -- Create_Title --
   ------------------

   function Create_Title
     (Sector : Concorde.Db.World_Sector_Reference;
      Zone   : Commodity_Reference;
      Price  : Concorde.Money.Price_Type)
      return Commodity_Reference
   is
      Tag : constant String :=
        Title_Tag (Sector, Zone);
   begin
      if not Exists (Tag) then
         declare
            Title : constant Concorde.Db.Sector_Title_Reference :=
              Concorde.Db.Sector_Title.Create
                (Commodity_Class =>
                   Concorde.Db.Commodity_Class.Get_Reference_By_Tag ("title"),
                 Index           => Natural (Vector.Last_Index) + 1,
                 Initial_Price   => Price,
                 Mass            => 0.1,
                 Density         => 0.1,
                 Tag             => Tag,
                 Zone            =>
                   Concorde.Db.Zone.Get_Zone
                     (To_Database_Reference (Zone))
                 .Get_Zone_Reference,
                 World_Sector    => Sector);
         begin
            Add_To_Cache (Concorde.Db.Sector_Title.Get (Title));
         end;

      end if;

      return Get (Tag);
   end Create_Title;

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
      return Get_Commodity
        (Concorde.Db.Commodity.Get_Reference_By_Tag (Tag));
   end Get;

   -------------------
   -- Get_Commodity --
   -------------------

   function Get_Commodity
     (Commodity : Concorde.Db.Commodity_Reference)
      return Commodity_Reference
   is
      use type Concorde.Db.Commodity_Reference;
   begin
      Check_Cache;
      for I in 1 .. Vector.Last_Index loop
         if Vector.Element (I).Reference = Commodity then
            return I;
         end if;
      end loop;
      raise Constraint_Error with
        "no such commodity:" & Db.To_String (Commodity);
   end Get_Commodity;

   -------------------
   -- Get_Commodity --
   -------------------

   function Get_Commodity
     (Title : Concorde.Db.Sector_Title_Reference)
      return Commodity_Reference
   is
      use type Concorde.Db.Sector_Title_Reference;
   begin
      Check_Cache;
      for I in 1 .. Vector.Last_Index loop
         if Vector.Element (I).Sector_Title_Reference = Title then
            return I;
         end if;
      end loop;
      raise Constraint_Error with
        "no such commodity:" & Db.To_String (Title);
   end Get_Commodity;

   -------------------
   -- Get_Commodity --
   -------------------

   function Get_Commodity
     (Zone : Concorde.Db.Zone_Reference)
      return Commodity_Reference
   is
      use type Concorde.Db.Zone_Reference;
   begin
      Check_Cache;
      for I in 1 .. Vector.Last_Index loop
         if Vector.Element (I).Zone_Reference = Zone then
            return I;
         end if;
      end loop;
      raise Constraint_Error with
        "no such commodity:" & Db.To_String (Zone);
   end Get_Commodity;

   ------------------------
   -- Get_Price_Per_Item --
   ------------------------

   function Get_Price_Per_Item
     (Stock     : Stock_Type;
      Commodity : Concorde.Commodities.Commodity_Reference)
      return Concorde.Money.Price_Type
   is
      use Concorde.Quantities;
      Rec : constant Stock_Record :=
        Stock.Get_Rec (Commodity);
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
        (Has_Properties => Vector (Commodity).Has_Properties,
         Property       => Property)
        .Value;
   end Get_Property;

   ------------------
   -- Get_Quantity --
   ------------------

   function Get_Quantity
     (Stock     : Stock_Type;
      Commodity : Commodity_Reference)
      return Concorde.Quantities.Quantity_Type
   is
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
      Commodity : Commodity_Reference)
      return Stock_Record
   is
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
      Commodity : Commodity_Reference)
      return Concorde.Money.Money_Type
   is
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
        (Has_Properties =>
           Vector (Commodity).Has_Properties,
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
      Check_Cache;
      return Vector (Commodity).Initial_Price;
   end Initial_Price;

   --------------
   -- Is_Lease --
   --------------

   function Is_Lease
     (Commodity : Commodity_Reference)
      return Boolean
   is
   begin
      return Vector (Commodity).Is_Lease;
   end Is_Lease;

   --------------
   -- Is_Skill --
   --------------

   function Is_Skill (Commodity : Commodity_Reference) return Boolean is
   begin
      return Vector (Commodity).Is_Skill;
   end Is_Skill;

   --------------
   -- Is_Title --
   --------------

   function Is_Title
     (Commodity : Commodity_Reference)
      return Boolean
   is
   begin
      return Vector (Commodity).Is_Title;
   end Is_Title;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Stock   : Stock_Type;
      Process : not null access
        procedure (Commodity : Commodity_Reference;
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
      return Commodity_Array
   is
      Leases : Commodity_Subset_Vectors.Vector;
   begin
      Check_Cache;
      for Item of Leases_Vector loop
         if Vector (Item).Leased_Commodity = For_Commodity then
            Leases.Append (Item);
         end if;
      end loop;
      return To_Array (Leases);
   end Lease_Commodities;

   ----------------
   -- Lease_Days --
   ----------------

   function Lease_Days
     (Commodity : Commodity_Reference)
      return Positive
   is
   begin
      return Vector (Commodity).Lease_Days;
   end Lease_Days;

   ----------------------
   -- Leased_Commodity --
   ----------------------

   function Leased_Commodity
     (Commodity : Commodity_Reference)
      return Commodity_Reference
   is
   begin
      return Vector (Commodity).Leased_Commodity;
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
              (Get_Commodity (Item.Commodity), Item.Quantity, Item.Value);
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
   begin
      return Ada.Strings.Unbounded.To_String (Vector (Commodity).Local_Name);
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
      Commodity : Commodity_Reference;
      Quantity  : Concorde.Quantities.Quantity_Type;
      Value     : Concorde.Money.Money_Type)
   is
      use Concorde.Money, Concorde.Quantities;
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
      Commodity : Commodity_Reference;
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

   function Skills return Commodity_Array is
   begin
      Check_Cache;
      return To_Array (Skills_Vector);
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
      return Get_Commodity
        (Concorde.Db.Commodity.Get_Reference_By_Tag
           (Title_Tag (Sector, Title_Zone)));
   end Title_Commodity;

   ---------------
   -- Title_Tag --
   ---------------

   function Title_Tag
     (Sector     : Concorde.Db.World_Sector_Reference;
      Title_Zone : Commodity_Reference)
      return String
   is
   begin
      return "T--"
        & Tag (Title_Zone)
        & "-"
        & Ada.Strings.Fixed.Trim
        (Concorde.Db.To_String (Sector), Ada.Strings.Both);
   end Title_Tag;

   --------------
   -- To_Array --
   --------------

   function To_Array
     (Vector : Commodity_Subset_Vectors.Vector)
      return Commodity_Array
   is
   begin
      return Arr : Commodity_Array (1 .. Vector.Last_Index) do
         for I in Arr'Range loop
            Arr (I) := Vector (I);
         end loop;
      end return;
   end To_Array;

   ---------------------------
   -- To_Database_Reference --
   ---------------------------

   function To_Database_Reference
     (Commodity : Commodity_Reference)
      return Concorde.Db.Commodity_Reference
   is
   begin
      Check_Cache;
      return Vector (Commodity).Reference;
   end To_Database_Reference;

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
