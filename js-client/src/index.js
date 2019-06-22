import axios from 'axios';

class Node {

  constructor(id, name) {
    this.id = id;
    this.name = name || null;
    this.edgesFrom = []; // this -> other
    this.edgesTo = []; // other -> this
  }

  _pushEdgesFrom(label, right) {
    if (!this.edgesFrom.find(([b, r]) => b === label && r === right))
      this.edgesFrom.push([label, right]);
  }

  _pushEdgesTo(label, left) {
    if (!this.edgesTo.find(([b, l]) => b === label && l === left))
      this.edgesTo.push([label, left]);
  }

  _removeEdgesFrom(label, right) {
    const index = this.edgesFrom.findIndex(([b, r]) => b === label && r === right);
    if (index !== -1)
      this.edgesFrom.splice(index, 1);
  }

  _removeEdgesTo(label, left) {
    const index = this.edgesTo.findIndex(([b, l]) => b === label && l === left);
    if (index !== -1)
      this.edgesTo.splice(index, 1);
  }

  get isSymbol() {
    return !!this.id;
  }

  get isString() {
    return !this.id;
  }

  toString() {
    return this.isSymbol ? this.id : JSON.stringify(this.name);
  }

}

function makeGen(f) {
  return new Proxy({}, {get(_, name) { return f(name); }});
}

export class YamiClient {

  constructor(opts) {
    this.url = opts.url;
    this.symbols = {};
    this.strings = {};
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
        return x.toString();
      if (typeof x === 'string')
        return JSON.stringify(x);
      x.bound = 1;
      return x.name;
    };
    fn({
      client: this,
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
        const g = x => x[0] === '"' ? this.client.getString(JSON.parse(x)) : this.client.getSymbol(x);
        collectCb['"' + collectCount + '"'] = (...xs) => f(...xs.map(g));
        collectCount += 1;
      }
    });
    const res = await this.sendRawQuery(commands.join('\n'));
    this.formatResult(res).map(x => collectCb[x[0]](...x.slice(1)));
    return res; // TODO
  }

  async fetchAsVertex(node) {
    if (typeof node === 'string')
      node = this.getString(node);

    await this.query(s => {
      const {label, right} = s.var;
      s.findAll(label, node, right);
      s.collect((label, right) => {
        if (!node.edgesFrom.find(([b, r]) => b === label && r === right))
          node.edgesFrom.push([label, right]);
        right._pushEdgesTo(label, node);
      });
    });

    await this.query(s => {
      const {label, left} = s.var;
      s.findAll(label, left, node);
      s.collect((label, left) => {
        if (!node.edgesTo.find(([b, l]) => b === label && l === left))
          node.edgesTo.push([label, left]);
        left._pushEdgesFrom(label, node);
      });
    });

    return node;
  }

  // fetchAsLabel(node) {}

  async addEdge(label, left, right) {
    if (typeof label === 'string') label = this.getString(label);
    if (typeof left === 'string') left = this.getString(left);
    if (typeof right === 'string') right = this.getString(right);
    let succeeded = false;
    await this.query(s => {
      if (label === null) label = s.symbol.b;
      if (left === null) left = s.symbol.l;
      if (right === null) right = s.symbol.r;
      s.add(label, left, right);
      s.collect(() => {
        succeeded = true;
      });
      if (label.type === 'symbol') s.collect((b) => { label = b; });
      if (left.type === 'symbol') s.collect((l) => { left = l; });
      if (right.type === 'symbol') s.collect((r) => { right = r; });
    });
    if (succeeded) {
      left._pushEdgesFrom(label, right);
      right._pushEdgesTo(label, left);
    }
    return succeeded && [label, left, right];
  }

  async removeEdge(label, left, right) {
    if (typeof label === 'string') label = this.getString(label);
    if (typeof left === 'string') left = this.getString(left);
    if (typeof right === 'string') right = this.getString(right);
    let succeeded = false;
    await this.query(s => {
      s.rm1(label, left, right);
      s.collect(() => {
        succeeded = true;
      });
    });
    if (succeeded) {
      left._removeEdgesFrom(label, right);
      right._removeEdgesTo(label, left);
    }
    return succeeded && [label, left, right];
  }

  getSymbol(id, name) {
    if (this.symbols[id])
      return this.symbols[id];
    return this.symbols[id] = new Node(id, name);
  }

  getString(string) {
    if (this.strings[string])
      return this.strings[string];
    return this.strings[string] = new Node(null, string);
  }

}
