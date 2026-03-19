import { NextApiRequest, NextApiResponse } from "next"

export default async (req: NextApiRequest, res: NextApiResponse) => {
  const r = await fetch('http://localhost:5000/', {method: 'POST', body: req.body})
  res.status(r.status).send(await r.text())
}
