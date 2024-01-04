import { useCallback, useState } from "react";
import { API_ENDPOINT } from "../../config";
import { fetcher } from "../../fetcher";

export const Adder = ({ query }: { query?: string }) => {
  const [rawCode, setRawCode] = useState(query ?? 'a b c')
  const [status, setStatus] = useState('')
  const [csv, setCsv] = useState('')

  const handleAdd = useCallback(async () => {
    const p = rawCode.split(/\s+/g)
    if (p.length !== 3)
      return;

    const a: any[] = p.map(x => {
      if (x[0] === "$")
        return +x.slice(1)
      return parse(x)
    });

    let code;
    if (a.every(x => typeof x !== "number")) {
      const rows = csv.trim().split('\n').map(x => x.split(',').map(x => x.trim()).filter(x => x !== ''));
      if (rows.length === 0) return;
      if (!rows.every(row => row.length === rows[0].length)) return;
      const parsedRows = rows.map(row => row.map(x => JSON.stringify(x)));
      code = `(and ${parsedRows.map(row =>
        `(sym (${a.filter(x => x.type === 'var').map(x => x.rep).filter((x, i, a) => a.indexOf(x) === i).join(' ')})
        (add ${f(a[0], row)} ${f(a[1], row)} ${f(a[2], row)}))`
      ).join(' ')})`
    } else {
      code = `(sym (${a.filter(x => x.type === 'var').map(x => x.rep).filter((x, i, a) => a.indexOf(x) === i).join(' ')})
        (add ${a[0].rep} ${a[1].rep} ${a[2].rep}))`
    }
    const res = await fetcher(API_ENDPOINT, code)
    setStatus(res + '')
  }, [csv, rawCode])

  const handleRemove = useCallback(async () => {
    const p = rawCode.split(/\s+/g)
    if (p.length === 3) {
      const a = p.map(parse)
      const code = `(rm ${a[0].rep} ${a[1].rep} ${a[2].rep})`
      const res = await fetcher(API_ENDPOINT, code)
      setStatus(res + '')
    }
  }, [rawCode])

  return (
    <div>
      <input type="text" value={rawCode} onChange={(e) => setRawCode(e.target.value)} />
      <br />
      <textarea value={csv} onChange={(e) => setCsv(e.target.value)}></textarea>
      <br />
      <button onClick={() => { handleAdd() }}>add</button>
      <button onClick={() => { handleRemove() }}>remove</button>
      <div>
        {status}
      </div>
    </div>
  );
}

function parse(x: string): { type: "sym" | "str" | "var", rep: string } {
  if (x[0] === ':')
    return { type: 'sym', rep: x };
  if (x[0] === '"')
    return { type: 'str', rep: x };
  return { type: 'var', rep: x };
}

function f(x: any, row: any[]) {
  if (typeof x === "number") return row[x];
  return x.rep;
}
