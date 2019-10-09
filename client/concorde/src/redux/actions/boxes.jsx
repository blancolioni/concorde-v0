import { SPLIT, SET_CLIENT } from '../actionTypes';

export const splitVertical = boxId => ({
    type: SPLIT,
    payload: {
        vertical: true,
        boxId: boxId,
        },
    });

export const setClient = contents => ({
    type: SET_CLIENT,
    payload: {
        contents,
        },
    });
    