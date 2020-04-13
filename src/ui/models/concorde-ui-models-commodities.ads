with Nazar.Models.Table;

with Concorde.Db;

package Concorde.UI.Models.Commodities is

   function Commodity_Market_Model
     (Commodity : Concorde.Db.Commodity_Reference)
      return Nazar.Models.Table.Nazar_Table_Model;

end Concorde.UI.Models.Commodities;
