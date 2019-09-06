with Concorde.Contexts.Containers;

with Concorde.Calendar;
with Concorde.Commodities;
with Concorde.Money;
with Concorde.Quantities;
with Concorde.Worlds;

with Concorde.Db.Commodity;
with Concorde.Db.Market;
with Concorde.Db.Transaction;

package body Concorde.Contexts.Markets is

   type Market_Context_Type is
     new Root_Context_Type with
      record
         Market : Concorde.Db.Market_Reference;
      end record;

   overriding function Is_Valid
     (Context : Market_Context_Type)
      return Boolean;

   overriding procedure Get_Child_Contexts
     (Context  : Market_Context_Type;
      Children : in out Context_List'Class);

   overriding function Class
     (Context : Market_Context_Type)
      return String
   is ("market");

   overriding function Name
     (Context : Market_Context_Type)
      return String
   is (Concorde.Worlds.Name
       (Concorde.Db.Market.Get (Context.Market).World));

   type Market_Commodity_Context_Type is
     new Root_Context_Type with
      record
         Market    : Concorde.Db.Market_Reference;
         Commodity : Concorde.Db.Commodity_Reference;
      end record;

   overriding function Is_Valid
     (Context : Market_Commodity_Context_Type)
      return Boolean;

   overriding procedure Get_Child_Contexts
     (Context  : Market_Commodity_Context_Type;
      Children : in out Context_List'Class);

   overriding function Class
     (Context : Market_Commodity_Context_Type)
      return String
   is ("market-commodity");

   overriding function Name
     (Context : Market_Commodity_Context_Type)
      return String
   is (Concorde.Commodities.Local_Name (Context.Commodity));

   overriding procedure Iterate_Content_Lines
     (Context : Market_Commodity_Context_Type;
      Process : not null access
        procedure (Line : String));

   function Market_Commodity_Context
     (Market : Concorde.Db.Market_Reference;
      Commodity : Concorde.Db.Commodity_Reference)
      return Context_Type;

   procedure Scan_Markets
     (Unused  : Boolean;
      Process : not null access
        procedure (Context : Context_Type));

   package Market_Containers is
     new Concorde.Contexts.Containers
       (Container_Name    => "markets",
        Context_Reference => Boolean,
        Iterate_Children  => Scan_Markets);

   ------------------------
   -- Get_Child_Contexts --
   ------------------------

   overriding procedure Get_Child_Contexts
     (Context  : Market_Context_Type;
      Children : in out Context_List'Class)
   is
   begin
      Children.Clear;
      for Commodity of Concorde.Db.Commodity.Scan_By_Tag loop
         for Offer of
           Concorde.Db.Transaction.Select_Transaction_Bounded_By_Time_Stamp
             (Context.Market, Commodity.Get_Commodity_Reference,
              Concorde.Calendar.Start, Concorde.Calendar.Clock)
         loop
            Children.Append
              (Market_Commodity_Context
                 (Context.Market, Commodity.Get_Commodity_Reference));
            exit;
         end loop;
      end loop;
   end Get_Child_Contexts;

   ------------------------
   -- Get_Child_Contexts --
   ------------------------

   overriding procedure Get_Child_Contexts
     (Context  : Market_Commodity_Context_Type;
      Children : in out Context_List'Class)
   is
      pragma Unreferenced (Context);
   begin
      Children.Clear;
   end Get_Child_Contexts;

   --------------
   -- Is_Valid --
   --------------

   overriding function Is_Valid
     (Context : Market_Context_Type)
      return Boolean
   is
      use type Concorde.Db.Market_Reference;
   begin
      return Context.Market /= Concorde.Db.Null_Market_Reference;
   end Is_Valid;

   --------------
   -- Is_Valid --
   --------------

   overriding function Is_Valid
     (Context : Market_Commodity_Context_Type)
      return Boolean
   is
      use Concorde.Db;
   begin
      return Context.Market /= Null_Market_Reference
        and then Context.Commodity /= Null_Commodity_Reference;
   end Is_Valid;

   ---------------------------
   -- Iterate_Content_Lines --
   ---------------------------

   overriding procedure Iterate_Content_Lines
     (Context : Market_Commodity_Context_Type;
      Process : not null access
        procedure (Line : String))
   is
      use Concorde.Money, Concorde.Quantities;
      Total_Trade : Quantity_Type := Zero;
      Last_Trade  : Quantity_Type := Zero;
      Last_Price  : Price_Type    := Zero;
   begin
      for Transaction of
        Concorde.Db.Transaction.Select_Transaction_Bounded_By_Time_Stamp
          (Context.Market, Context.Commodity,
           Concorde.Calendar.Start, Concorde.Calendar.Clock)
      loop
         Total_Trade := Total_Trade + Transaction.Quantity;
         Last_Trade := Transaction.Quantity;
         Last_Price := Transaction.Price;
      end loop;

      Process ("total: " & Show (Total_Trade));
      Process ("last quantity: " & Show (Last_Trade));
      Process ("last price: " & Show (Last_Price));

   end Iterate_Content_Lines;

   ------------------------------
   -- Market_Commodity_Context --
   ------------------------------

   function Market_Commodity_Context
     (Market    : Concorde.Db.Market_Reference;
      Commodity : Concorde.Db.Commodity_Reference)
      return Context_Type
   is
   begin
      return Market_Commodity_Context_Type'
        (Market    => Market,
         Commodity => Commodity);
   end Market_Commodity_Context;

   --------------------
   -- Market_Context --
   --------------------

   function Market_Context
     (Market : Concorde.Db.Market_Reference)
      return Context_Type
   is
   begin
      return Market_Context_Type'
        (Market => Market);
   end Market_Context;

   ------------------
   -- Scan_Markets --
   ------------------

   procedure Scan_Markets
     (Unused  : Boolean;
      Process : not null access
        procedure (Context : Context_Type))
   is
   begin
      for Market of
        Concorde.Db.Market.Scan_By_Top_Record
      loop
         Process (Market_Context (Market.Get_Market_Reference));
      end loop;
   end Scan_Markets;

   -------------------------
   -- Top_Level_Container --
   -------------------------

   function Top_Level_Container return Context_Type is
   begin
      return Market_Containers.Container_Context (True);
   end Top_Level_Container;

end Concorde.Contexts.Markets;
