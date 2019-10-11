import { INIT_BOXES, SPLIT, SET_CLIENT, LOGOUT } from '../actionTypes';
import { Box, splitVertical, setChildComponent } from '../../Dashboard/Box';

const initialState = {    
    nextId: 0,
    boxes: false,
    }

export default function reducer(state = initialState, action) {
    switch (action.type) {
        case SPLIT:
            console.log('SPLIT', action.payload, state.boxes)
            let splitBox = state.boxes[action.payload.boxId];
            let [newParent, topChild, bottomChild] = splitVertical(splitBox, state.nextId, state.nextId + 1);
            return {
                ...state,
                nextId: state.nextId + 2,
                boxes: {
                    ...state.boxes,
                    [action.payload.boxId]: newParent,
                    [state.nextId]: topChild,
                    [state.nextId+1]: bottomChild,
                }
            }

        case SET_CLIENT:
            const {boxId, title, view, model, client } = action.payload.contents;
            let oldBox = state.boxes[boxId];
            let newBox = new Box(boxId, oldBox.anchor);
            setChildComponent(newBox, title, view, model, client);

            console.log("set-client", newBox);
            return {
                ...state,
                boxes: {
                    ...state.boxes,
                    [boxId]: newBox,
                }
            }

        case INIT_BOXES:
            return {
                ...state,
                nextId: action.payload.contents.nextId,
                boxes: action.payload.contents.boxes,
            }

        case LOGOUT:
            return initialState;

        default:
            return state;
    }
}
