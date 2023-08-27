import React, { useCallback, useContext, useEffect, useState, VFC } from "react";
import { API_ENDPOINT } from "../../config";
import { StateContext } from "../../contexts/state";
import { fetcher } from "../../fetcher";
import { Node } from "../../models";
import { parseYamiResponse } from "../../parse";
import './index.css';
import { useStore } from "../../state";

export const TableView: VFC = () => {
  const [rawCode, setRawCode] = useState('a b c');
  const [query, setQuery] = useState<[Node, Node, Node] | null>(null);
  const [status, setStatus] = useState('');
  const [rows, setRows] = useState<Node[][]>([]);
  const { state, dispatch } = useContext(StateContext);
  const { represent, signifySymbol, setSignifySymbol } = useStore();

  const [contextMenuState, setContextMenuState] = useState({ show: false, x: 0, y: 0, select: [0, 0] });

  const handleSend = useCallback(async () => {
    const p = rawCode.split(/\s+/g)
    if (p.length === 3) {
      const a = p.map(x => new Node(x))
      setQuery(a as [Node, Node, Node]);
      const code = `(find ${a[0].rep} ${a[1].rep} ${a[2].rep} (and ${a.map(x => `(print ${x.rep})`).join(' ')}))`
      const res = await fetcher(API_ENDPOINT, code)
      setStatus(res[0] + '')
      if (res[0] === 200) {
        const arr = parseYamiResponse(res[1] + '')
        const rows = []
        while (arr.length) {
          rows.push([arr.shift(), arr.shift(), arr.shift()] as Node[])
        }
        setRows(rows)
      }
    }
  }, [rawCode])

  useEffect(() => {
    const listener = () => {
      setContextMenuState((x) => ({ ...x, show: false }));
    }
    window.addEventListener('click', listener);
    return () => {
      window.removeEventListener('click', listener);
    }
  }, [])

  const handleContextMenu = (e: React.MouseEvent<HTMLTableElement, MouseEvent>) => {
    if (contextMenuState.show) {
      setContextMenuState((x) => ({ ...x, show: false }));
    } else if (e.target instanceof HTMLElement && e.target.dataset.node) {
      setContextMenuState({ show: true, x: e.clientX, y: e.clientY, select: e.target.dataset.node.split('-').map(x => +x) });
      e.preventDefault();
    }
  };

  const handleSetSignifySymbol = useCallback(() => {
    const index = rows[contextMenuState.select[0]][contextMenuState.select[1]].index;
    if (index !== null) setSignifySymbol(index);
    setContextMenuState((x) => ({ ...x, show: false }));
  }, [rows, contextMenuState])

  const handleListAsRelation = useCallback(() => {
    const rep = rows[contextMenuState.select[0]][contextMenuState.select[1]].rep;
    setRawCode(rep + ' b c');
    setContextMenuState((x) => ({ ...x, show: false }));
  }, [rows, contextMenuState])

  const writeDown = useCallback(() => {
    const text = rows.filter(row => row[0].index !== signifySymbol).map(row => [represent(row[1]), represent(row[0]), represent(row[2])].join(' ') + '.').join('\n');
    console.log(text);
  }, [rows]);

  return (
    <div>
      <input type="text" value={rawCode} onChange={(e) => setRawCode(e.target.value)} />
      <button onClick={() => { handleSend() }}>send</button>
      <div>
        {status}

        <button onClick={writeDown}>write down</button>
        <table className="table" onContextMenu={e => handleContextMenu(e)}>
          <thead>
            <tr>
              <th>{query?.[0].rep}</th>
              <th>{query?.[1].rep}</th>
              <th>{query?.[2].rep}</th>
            </tr>
          </thead>
          <tbody>
            {rows.map((x, i) =>
              <tr key={i}>
                <td data-node={i + '-' + 0} onClick={() => dispatch({ type: 'setCurrent', node: x[0] })}>
                  {represent(x[0])}
                </td>
                <td data-node={i + '-' + 1} onClick={() => dispatch({ type: 'setCurrent', node: x[1] })}>
                  {represent(x[1])}
                </td>
                <td data-node={i + '-' + 2} onClick={() => dispatch({ type: 'setCurrent', node: x[2] })}>
                  {represent(x[2])}
                </td>
              </tr>)}
            <tr>
              <td className="addButton" colSpan={3}>
                +
              </td>
            </tr>
          </tbody>
        </table>
        <div className="contextMenu"
          style={{
            display: contextMenuState.show ? 'inline-block' : 'none',
            left: contextMenuState.x + 'px', top: contextMenuState.y + 'px',
          }}>
          <div onClick={e => e.stopPropagation()}>
            {contextMenuState.select[0]}-{contextMenuState.select[1]}
          </div>
          <div onClick={handleSetSignifySymbol}>
            set signify symbol {signifySymbol ? `(${signifySymbol})` : ''}
          </div>
          <div onClick={handleListAsRelation}>
            list as relation
          </div>
        </div>
      </div>
    </div>
  );
}
