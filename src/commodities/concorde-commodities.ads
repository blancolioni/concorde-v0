with Ada.Containers.Doubly_Linked_Lists;

with Concorde.Properties;

with Concorde.Money;
with Concorde.Quantities;

with Concorde.Db;

package Concorde.Commodities is

   type Commodity_Reference is private;

   function To_Database_Reference
     (Commodity : Commodity_Reference)
      return Concorde.Db.Commodity_Reference;

   function Get_Commodity
     (Commodity : Concorde.Db.Commodity_Reference)
      return Commodity_Reference;

   function Get_Commodity
     (Title : Concorde.Db.Sector_Title_Reference)
      return Commodity_Reference;

   function Get_Commodity
     (Zone : Concorde.Db.Zone_Reference)
      return Commodity_Reference;

   function Local_Name
     (Commodity : Commodity_Reference)
      return String;

   function Initial_Price
     (Commodity : Commodity_Reference)
      return Concorde.Money.Price_Type;

   function Has_Property
     (Commodity : Commodity_Reference;
      Property  : Concorde.Properties.Property_Type)
      return Boolean;

   function Get_Property
     (Commodity : Commodity_Reference;
      Property  : Concorde.Properties.Property_Type)
      return Real;

   function Exists
     (Tag : String)
      return Boolean;

   function Get
     (Tag : String)
      return Commodity_Reference;

   type Commodity_Array is array (Positive range <>) of Commodity_Reference;

   function All_Commodities return Commodity_Array;
   function Skills return Commodity_Array;

   function Is_Skill (Commodity : Commodity_Reference) return Boolean;

   function Create_Title
     (Sector : Concorde.Db.World_Sector_Reference;
      Zone   : Commodity_Reference;
      Price  : Concorde.Money.Price_Type)
      return Commodity_Reference;

   function Lease_Commodities
     (For_Commodity : Commodity_Reference)
      return Commodity_Array;

   function Create_Lease
     (Commodity : Commodity_Reference;
      Days      : Positive)
      return Commodity_Reference;

   function Is_Lease
     (Commodity : Commodity_Reference)
      return Boolean;

   function Leased_Commodity
     (Commodity : Commodity_Reference)
      return Commodity_Reference
     with Pre => Is_Lease (Commodity);

   function Lease_Days
     (Commodity : Commodity_Reference)
      return Positive
     with Pre => Is_Lease (Commodity);

   type Commodity_Class_Reference is private;

   function Lease_Category
     return Concorde.Db.Commodity_Group_Reference;

   function Title_Category
     return Concorde.Db.Commodity_Group_Reference;

   function Title_Tag
     (Sector     : Concorde.Db.World_Sector_Reference;
      Title_Zone : Commodity_Reference)
      return String;

   function Title_Commodity
     (Sector     : Concorde.Db.World_Sector_Reference;
      Title_Zone : Commodity_Reference)
      return Commodity_Reference;

   function Is_Title
     (Commodity : Commodity_Reference)
      return Boolean;

   type Stock_Type is tagged private;

   procedure Clear (Stock : in out Stock_Type);

   function Get_Quantity
     (Stock     : Stock_Type;
      Commodity : Commodity_Reference)
      return Concorde.Quantities.Quantity_Type;

   function Get_Value
     (Stock     : Stock_Type;
      Commodity : Commodity_Reference)
      return Concorde.Money.Money_Type;

   function Get_Price_Per_Item
     (Stock     : Stock_Type;
      Commodity : Commodity_Reference)
      return Concorde.Money.Price_Type;

   procedure Set_Quantity
     (Stock     : in out Stock_Type;
      Commodity : Commodity_Reference;
      Quantity  : Concorde.Quantities.Quantity_Type;
      Value     : Concorde.Money.Money_Type);

   procedure Set_Quantity
     (Stock     : in out Stock_Type;
      Commodity : Commodity_Reference;
      Quantity  : Concorde.Quantities.Quantity_Type;
      Price_Per : Concorde.Money.Price_Type);

   procedure Add_Quantity
     (Stock     : in out Stock_Type;
      Commodity : Commodity_Reference;
      Quantity  : Concorde.Quantities.Quantity_Type;
      Price_Per : Concorde.Money.Price_Type);

   function Total_Quantity
     (Stock     : Stock_Type)
      return Concorde.Quantities.Quantity_Type;

   function Total_Value
     (Stock     : Stock_Type)
      return Concorde.Money.Money_Type;

   procedure Iterate
     (Stock : Stock_Type;
      Process : not null access
        procedure (Commodity : Commodity_Reference;
                   Quantity  : Concorde.Quantities.Quantity_Type;
                   Value     : Concorde.Money.Money_Type));

   function Missing
     (Available_Stock : Stock_Type;
      Required_Stock  : Stock_Type)
      return Stock_Type;

   procedure Add
     (To    : in out Stock_Type;
      Other : Stock_Type);

   procedure Load
     (Stock     : in out Stock_Type;
      Has_Stock : Concorde.Db.Has_Stock_Reference);

private

   type Commodity_Reference is new Positive;

   type Commodity_Class_Reference is new Positive;

   type Stock_Record is
      record
         Commodity : Commodity_Reference;
         Quantity  : Concorde.Quantities.Quantity_Type;
         Value     : Concorde.Money.Money_Type;
      end record;

   package Stock_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Stock_Record);

   type Stock_Type is tagged
      record
         List           : Stock_Lists.List;
         Total_Quantity : Concorde.Quantities.Quantity_Type :=
                            Concorde.Quantities.Zero;
         Total_Value    : Concorde.Money.Money_Type :=
                            Concorde.Money.Zero;
      end record;

   function Tag
     (Commodity : Commodity_Reference)
      return String;

end Concorde.Commodities;
