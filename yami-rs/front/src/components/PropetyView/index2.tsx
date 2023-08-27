import React, { useCallback, useContext, useEffect, useState, VFC } from "react";
import { API_ENDPOINT } from "../../config";
import { StateContext } from "../../contexts/state";
import { fetcher } from "../../fetcher";
import { Node } from "../../models";
import { parseYamiResponse } from "../../parse";
import './index.css';

export const PropertyView: VFC = () => {
  const [rawQuery, setRawQuery] = useState('');
  const [query, setQuery] = useState<Node | null>(null);
  const [status, setStatus] = useState('');
  const [rows, setRows] = useState<Node[][][]>([[], []]);
  const { state, dispatch } = useContext(StateContext);

  const [contextMenuState, setContextMenuState] = useState({ show: false, x: 0, y: 0, select: [0, 0] });

  const handleSend = useCallback(async () => {
    const node = new Node(rawQuery);
    setQuery(node);
    const code1 = `(find a ${node.rep} c (and (print a) (print c)))`;
    const res1 = await fetcher(API_ENDPOINT, code1);
    setStatus(res1[0] + '');
    if (res1[0] !== 200) {
      return;
    }
    const rows1 = [];
    const arr1 = parseYamiResponse(res1[1] + '');
    while (arr1.length) {
      rows1.push([arr1.shift(), arr1.shift()] as Node[]);
    }

    const code2 = `(find a b ${node.rep} (and (print a) (print b)))`;
    const res2 = await fetcher(API_ENDPOINT, code2);
    setStatus(res2[0] + '');
    if (res2[0] !== 200) {
      return;
    }
    const rows2 = [];
    const arr2 = parseYamiResponse(res2[1] + '');
    while (arr2.length) {
      rows2.push([arr2.shift(), arr2.shift()] as Node[]);
    }

    setRows([rows1, rows2]);
  }, [rawQuery]);

  useEffect(() => {
    const listener = () => {
      setContextMenuState((x) => ({ ...x, show: false }));
    }
    window.addEventListener('click', listener);
    return () => {
      window.removeEventListener('click', listener);
    }
  }, []);

  const handleContextMenu = (e: React.MouseEvent<HTMLTableElement, MouseEvent>) => {
    // if (contextMenuState.show) {
    //   setContextMenuState((x) => ({ ...x, show: false }));
    // } else if (e.target instanceof HTMLElement && e.target.dataset.node) {
    //   setContextMenuState({ show: true, x: e.clientX, y: e.clientY, select: e.target.dataset.node.split('-').map(x => +x) });
    //   e.preventDefault();
    // }
  };

  // const handleSetSignifySymbol = useCallback(() => {
  //   setSignifySymbol(rows[contextMenuState.select[0]][contextMenuState.select[1]].rep);
  //   setContextMenuState((x) => ({ ...x, show: false }));
  // }, [rows, contextMenuState])

  // const handleListAsRelation = useCallback(() => {
  //   const rep = rows[contextMenuState.select[0]][contextMenuState.select[1]].rep;
  //   setRawCode(rep + ' b c');
  //   setContextMenuState((x) => ({ ...x, show: false }));
  // }, [rows, contextMenuState])

  return (
    <div>
      <input type="text" value={rawQuery} onChange={(e) => setRawQuery(e.target.value)} />
      <button onClick={() => { handleSend() }}>send</button>
      <div>
        {status}
        <table className="table" onContextMenu={e => handleContextMenu(e)}>
          <thead>
            <tr>
              <th>-</th>
              <th>-</th>
              <th>-</th>
            </tr>
          </thead>
          <tbody>
            {rows[0].map((x, i) =>
              <tr key={i}>
                <td data-node={i + '-' + 0} onClick={() => dispatch({ type: 'setCurrent', node: x[0] })}>
                  {x[0].rep}
                </td>
                <td data-node={i + '-' + 1} onClick={() => dispatch({ type: 'setCurrent', node: query as Node })}>
                  {query?.rep}
                </td>
                <td data-node={i + '-' + 2} onClick={() => dispatch({ type: 'setCurrent', node: x[1] })}>
                  {x[1].rep}
                </td>
              </tr>)}
              
            {rows[1].map((x, i) =>
              <tr key={i + rows[0].length}>
                <td data-node={i + '-' + 0} onClick={() => dispatch({ type: 'setCurrent', node: x[0] })}>
                  {x[0].rep}
                </td>
                <td data-node={i + '-' + 1} onClick={() => dispatch({ type: 'setCurrent', node: x[1] })}>
                  {x[1].rep}
                </td>
                <td data-node={i + '-' + 2} onClick={() => dispatch({ type: 'setCurrent', node: query as Node })}>
                  {query?.rep}
                </td>
              </tr>)}
          </tbody>
        </table>

        {/* <div className="contextMenu"
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
        </div> */}
      </div>
    </div>
  );
}
