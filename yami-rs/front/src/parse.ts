// export type YamiResponseElm = {type: 'sym', index: number} | {type: 'str', string: string}

import { Node } from "./models";

// export function parseYamiResponse(src: string): YamiResponseElm[] {
//   return src.trim().split('\n').map(x => {
//     if (x[0] === ':') {
//       return { type: 'sym', index: +x.slice(1) }
//     }
//     if (x[0] === '"') {
//       return { type: 'str', string: JSON.parse(x) }
//     }
//     throw new Error('invalid yami response')
//   })
// }
export function parseYamiResponse(src: string): Node[] {
  return src.trim().split('\n').filter(x => x.length).map(x => new Node(x))
}
