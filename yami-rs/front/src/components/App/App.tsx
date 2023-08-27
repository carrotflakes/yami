import { useContext, useEffect, useState } from 'react';
import useSWR from 'swr';
import './App.css';
import { Adder } from '../../components/Adder';
import { PropertyView } from '../../components/PropetyView';
import { TableView } from '../../components/TableView';
import { API_ENDPOINT } from '../../config';
import { StateContext } from '../../contexts/state';
import { fetcher } from '../../fetcher';
import { useStore } from '../../state';
import { parseYamiResponse } from '../../parse';
import Foldable from '../Foldable';

function App() {
  const [rawCode, setRawCode] = useState('(print "yo")')
  const [code, setCode] = useState('')
  const a = useSWR(['http://localhost:5000', code], fetcher, { shouldRetryOnError: false })
  const { state } = useContext(StateContext);
  const { represent } = useStore();

  useWatchSignifies();

  return (
    <div className="App">
      <div className="columns">
        <Foldable title="Free query" defaultOpen={true}>
          <textarea value={rawCode} onChange={(e) => setRawCode(e.target.value)}></textarea>
          <br />
          <button onClick={() => setCode(rawCode)}>send</button>
          <div>
            {a.data?.[1]}
          </div>
          <div>{code}</div>
        </Foldable>
        <Foldable title="Table view" defaultOpen={true}>
          <TableView />
        </Foldable>
        <Foldable title="Property view" defaultOpen={true}>
          <PropertyView node={state.currentNode} />
        </Foldable>
        <Foldable title="Add or remove" defaultOpen={true}>
          <Adder />
        </Foldable>
      </div>
      <button onClick={() => fetch(API_ENDPOINT + '/save')}>save</button>
      <div>
        {state.currentNode && represent(state.currentNode)}
        <br />
        {state.history.map(represent).join(', ')}
      </div>
    </div>
  );
}

export default App;

function useWatchSignifies() {
  const { signifySymbol, setSignifies } = useStore();
  useEffect(() => {
    if (signifySymbol !== null) {
      (async () => {
        const code = `(find :${signifySymbol} a b (and (print a) (print b)))`;
        const [status, res] = await fetcher(API_ENDPOINT, code);
        if (status === 200) {
          const arr = parseYamiResponse(res + '');
          const d: { [key: number]: string; } = {};
          while (arr.length) {
            const a = arr.shift();
            const b = arr.shift();
            if (a?.type === "str" && typeof b?.index === 'number') {
              d[b.index] = a?.string;
            }
          }
          setSignifies(d);
        }
      })();
    }
  }, [signifySymbol]);
}

