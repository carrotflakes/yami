
import { create } from "zustand";
import { devtools } from "zustand/middleware";
import { Node } from "./models";

export type State = {
  signifies: { [key: number]: string };
  signifySymbol: number | null;
  setSignifies: (signifies: { [key: number]: string }) => void;
  setSignifySymbol: (signifySymbol: number | null) => void;
  represent: (node: Node) => string;
}

export const useStore = create<State>()(
  devtools((set, get) => ({
    signifies: {},
    signifySymbol: null,
    setSignifies: (signifies: { [key: number]: string }) => set({ signifies }),
    setSignifySymbol: (signifySymbol: number | null) => set({ signifySymbol }),
    represent: (node: Node) => {
      if (node.index !== null) {
        const name = get().signifies[node.index];
        if (name)
          return '' + name;
      }
      return node.rep;
    }
  }))
)
