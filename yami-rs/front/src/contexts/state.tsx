import { createContext, FC, useReducer } from "react";
import { Node } from "../models";

type State = {
  currentNode: Node | null,
  history: Node[],
  selectCallback: ((node: Node) => void) | null,
};
type Action
  = { type: 'setCurrent', node: Node }
  | { type: 'select', node: Node }
  | { type: 'setSelectCallback', callback: ((node: Node) => void) | null };

export const StateContext = createContext<{ state: State, dispatch: React.Dispatch<Action> }>({ state: {} as State, dispatch: () => { } });

function reducer(state: State, action: Action): State {
  switch (action.type) {
    case 'setCurrent':
      return {
        ...state,
        currentNode: action.node,
        history: [...state.history, action.node],
      };
    case 'select':
      if (state.selectCallback) {
        state.selectCallback(action.node);
      }
      return {
        ...state,
      };
    case 'setSelectCallback':
      return {
        ...state,
        selectCallback: action.callback,
      };
  }
  return state;
}

export const StateProvider = ({ children }: { children: React.ReactNode }) => {
  const [state, dispatch] = useReducer(reducer, { currentNode: null, history: [], selectCallback: null });
  return (
    <StateContext.Provider value={{ state, dispatch }}>
      {children}
    </StateContext.Provider>
  )
}
