import React, { useCallback, useContext, useEffect, useMemo, useState, VFC } from "react";
import { API_ENDPOINT } from "../../config";
import { StateContext } from "../../contexts/state";
import { fetcher } from "../../fetcher";
import { Node } from "../../models";
import { parseYamiResponse } from "../../parse";
import './index.css';
import { useStore } from "../../state";

export const PropertyView: VFC<{ node: Node | null }> = ({ node }) => {
  const [status, setStatus] = useState('');
  const [rows, setRows] = useState<Node[][][]>([[], []]);
  const { state, dispatch } = useContext(StateContext);
  const { represent } = useStore();

  const [contextMenuState, setContextMenuState] = useState({ show: false, x: 0, y: 0, select: [0, 0] });

  useEffect(() => {
    (async () => {
      if (!node) {
        return;
      }
      const code1 = `(find a ${node.rep} c (and (print a) (print c)))`;
      const res1 = await fetcher(API_ENDPOINT, code1);
      if (res1[0] !== 200) {
        setStatus(res1[0] + '');
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
    })();
  }, [node]);

  useEffect(() => {
    const listener = () => {
      setContextMenuState((x) => ({ ...x, show: false }));
    }
    window.addEventListener('click', listener);
    return () => {
      window.removeEventListener('click', listener);
    }
  }, []);

  return (
    <div>
      <input type="text" value={node?.rep} />
      <div>
        {status}
        <table className="table">
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
                  {represent(x[0])}
                </td>
                <td data-node={i + '-' + 1} onClick={() => dispatch({ type: 'setCurrent', node: node as Node })}>
                  {node && represent(node)}
                </td>
                <td data-node={i + '-' + 2} onClick={() => dispatch({ type: 'setCurrent', node: x[1] })}>
                  {represent(x[1])}
                </td>
              </tr>)}

            {rows[1].map((x, i) =>
              <tr key={i + rows[0].length}>
                <td data-node={i + '-' + 0} onClick={() => dispatch({ type: 'setCurrent', node: x[0] })}>
                  {represent(x[0])}
                </td>
                <td data-node={i + '-' + 1} onClick={() => dispatch({ type: 'setCurrent', node: x[1] })}>
                  {represent(x[1])}
                </td>
                <td data-node={i + '-' + 2} onClick={() => dispatch({ type: 'setCurrent', node: node as Node })}>
                  {node && represent(node)}
                </td>
              </tr>)}
          </tbody>
        </table>
      </div>
    </div>
  );
}
