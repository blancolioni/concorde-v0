import { combineReducers } from "redux";

import login from "./login";
import boxes from "./boxes";

export default combineReducers({ login, boxes });
