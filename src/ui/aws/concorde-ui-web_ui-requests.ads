with Concorde.Game;
with Concorde.Json;

package Concorde.UI.Web_UI.Requests is

   function Game_State
     (Game : Concorde.Game.Game_Type)
      return Concorde.Json.Json_Value'Class;

   function Current_Phase
     (Game : Concorde.Game.Game_Type)
      return Concorde.Json.Json_Value'Class;

   function Faction_Names
     (Game : Concorde.Game.Game_Type)
      return Concorde.Json.Json_Value'Class;

   function Faction_State
     (Game : Concorde.Game.Game_Type)
      return Concorde.Json.Json_Value'Class;

   function Fleet_State
     (Game : Concorde.Game.Game_Type)
      return Concorde.Json.Json_Value'Class;

   function Legion_State
     (Game : Concorde.Game.Game_Type)
      return Concorde.Json.Json_Value'Class;

   function Republic_State
     (Game : Concorde.Game.Game_Type)
      return Concorde.Json.Json_Value'Class;

   function Continue
     (Game : in out Concorde.Game.Game_Type)
      return Concorde.Json.Json_Value'Class;

end Concorde.UI.Web_UI.Requests;
