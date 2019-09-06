with Concorde.Sessions;
with Concorde.UI.Layouts;

package Concorde.UI.Models.Dashboard is

   type Root_Dashboard_Model is
     new Root_Concorde_Model with private;

   function Layout
     (Model : Root_Dashboard_Model'Class)
      return Concorde.UI.Layouts.Layout_Type;

   function New_Tile
     (Model : in out Root_Dashboard_Model'Class)
      return Concorde.UI.Layouts.Tile_Index;

   procedure Delete_Tile
     (Model : in out Root_Dashboard_Model'Class;
      Deleted_Tile :  Concorde.UI.Layouts.Tile_Index;
      Replacement_Tile : out Concorde.UI.Layouts.Tile_Count);

   procedure Swap_Tiles
     (Model  : in out Root_Dashboard_Model'Class;
      Tile_1 : Concorde.UI.Layouts.Tile_Index;
      Tile_2 : Concorde.UI.Layouts.Tile_Index);

   type Dashboard_Model is access all Root_Dashboard_Model'Class;

   function Create_Dashboard_Model
     (Session : not null access Concorde.Sessions.Root_Concorde_Session'Class)
      return Dashboard_Model;

private

   type Root_Dashboard_Model is
     new Root_Concorde_Model with
      record
         Session   : Concorde.Sessions.Concorde_Session;
         Layout    : Concorde.UI.Layouts.Layout_Type;
         Algorithm : Concorde.UI.Layouts.Layout_Algorithm;
      end record;

   overriding function Title
     (Model : Root_Dashboard_Model)
      return String
   is (Model.Session.User_Name);

   function Layout
     (Model : Root_Dashboard_Model'Class)
      return Concorde.UI.Layouts.Layout_Type
   is (Model.Layout);

end Concorde.UI.Models.Dashboard;
