with Concorde.Db;
private with Concorde.Db.Commodity_Vectors;

with Concorde.Markets;

with Concorde.UI.Models.Tables;

package Concorde.UI.Models.Market is

   type Root_Market_Model is
     new Concorde.UI.Models.Tables.Root_Table_Model with private;

   type Market_Model is access all Root_Market_Model'Class;

   function Create
     (Market : Concorde.Db.Market_Reference)
      return Market_Model;

private

   package Commodity_Row_Vectors is
     new Concorde.Db.Commodity_Vectors
       (Concorde.UI.Models.Tables.Table_Row_Count, 0,
        Concorde.UI.Models.Tables."=");

   type Root_Market_Model is
     new Concorde.UI.Models.Tables.Root_Table_Model with
      record
         Reference         : Concorde.Db.Market_Reference;
         Market_Watcher_Id : Concorde.Markets.Market_Handler_Id;
         Commodity_Row     : Commodity_Row_Vectors.Vector;
      end record;

end Concorde.UI.Models.Market;
