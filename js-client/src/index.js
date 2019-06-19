import axios from 'axios';

class Node {

  constructor(id, name) {
    this.id = id;
    this.name = name || null;
    this.edgesFrom = [];
    this.edgesTo = [];
  }

  get isSymbol() {
    return !!this.id;
  }

  get isString() {
    return !this.id;
  }

  static getSymbol(id, name) {
    if (symbols[id])
      return symbols[id];
    return symbols[id] = new Node(id, name);
  }

  static getString(string) {
    if (strings[string])
      return strings[string];
    return strings[string] = new Node(null, string);
  }

}

const symbols = {
};
const strings = {
};

function makeGen(f) {
  return new Proxy({}, {get(_, name) { return f(name); }});
}

export class YamiClient {

  constructor(opts) {
    this.url = opts.url;
  }

  async sendRawQuery(query) {
    console.log(query);
    const res = await axios({
      method: 'POST',
      baseURL: this.url,
      url: '/api/v1/query',
      data: query
    });
    return res.data;
  }

  formatResult(result) {
    const l = result.split(/;\n/).map(x => x.split(/\s+/)); // FIXME
    l.pop();
    return l;
  }

  async query(fn) {
    const commands = [];
    let env = {};
    let collectCount = 0;
    const collectCb = {};
    const varString = x => {
      if (x instanceof Node)
        return x.isSymbol ? x.id : JSON.stringify(x.name);
      if (typeof x === 'string')
        return JSON.stringify(x);
      x.bound = 1;
      return x.name;
    };
    fn({
      common: makeGen(name => {
        commands.push(`common ${name};`);
        const v = {type: 'common', name, bound: 1};
        env = {...env, [name]: v};
        return v;
      }),
      symbol: makeGen(name => {
        commands.push(`symbol ${name};`);
        const v = {type: 'symbol', name, bound: 1};
        env = {...env, [name]: v};
        return v;
      }),
      locked: makeGen(name => {
        commands.push(`locked ${name} __${name}__;`);
        const v = {type: 'locked', name};
        env = {...env, [name]: v};
        return v;
      }),
      var: makeGen(name => {
        //commands.push(`var ${name};`);
        const v = {type: 'var', name};
        env = {...env, [name]: v};
        return v;
      }),
      //unlock: 1,
      add(x, y, z) {
        commands.push(`add ${varString(x)} ${varString(y)} ${varString(z)};`);
      },
      rm1(x, y, z) {
        commands.push(`rm1 ${varString(x)} ${varString(y)} ${varString(z)};`);
      },
      rmAll(x, y, z) {
        commands.push(`rmAll ${varString(x)} ${varString(y)} ${varString(z)};`);
      },
      find1(x, y, z) {
        commands.push(`find1 ${varString(x)} ${varString(y)} ${varString(z)};`);
      },
      findAll(x, y, z) {
        commands.push(`findAll ${varString(x)} ${varString(y)} ${varString(z)};`);
      },
      collect(f) {
        const params = f.toString().split('=>')[0].match(/[\w\d]+/g) || [];
        const env_ = env;
        commands.push(`collect "${collectCount}" ${params.join(' ')};`);
        const g = x => x[0] === '"' ? JSON.parse(x) : Node.getSymbol(x);
        collectCb['"' + collectCount + '"'] = (...xs) => f(...xs.map(g));
      }
    });
    const res = await this.sendRawQuery(commands.join('\n'));
    this.formatResult(res).map(x => collectCb[x[0]](...x.slice(1)));
    return res; // TODO
  }

}
