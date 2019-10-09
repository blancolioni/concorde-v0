import { SPLIT, SET_CLIENT } from '../actionTypes';
import Box from '../../Dashboard/Box';

const initialState = {
    nextId: 1,
    boxes: {
        0: new Box(0,1,1,13,13),
    },
}
export default function reducer(state = initialState, action) {
    console.log("box-action", state,action);
    switch (action.type) {
        case SPLIT:
            let splitBox = state.boxes[action.payload.boxId];
            let [newParent, topChild, bottomChild] = splitBox.splitVertical(state.nextId, state.nextId + 1);
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
            console.log("set-client", action);
            const {boxId, title, view, model, client } = action.payload.contents;
            let oldBox = state.boxes[boxId];
            let newBox = new Box(boxId, oldBox.anchor);
            newBox.setChildComponent(title, view, model, client);

            console.log("set-client", newBox);
            return {
                ...state,
                boxes: {
                    ...state.boxes,
                    [boxId]: newBox,
                }
            }

        default:
            return state;
    }
}
