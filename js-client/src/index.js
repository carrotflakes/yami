import axios from 'axios';

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
    return this.formatResult(res.data);
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
        const f = x => (typeof x === 'string' ? JSON.stringify(x) : x.name);
        commands.push(`add ${f(x)} ${f(y)} ${f(z)};`);
      },
      rm(x, y, z) {
        const f = x => (typeof x === 'string' ? JSON.stringify(x) : (x.bound = 1, x.name));
        commands.push(`rm ${f(x)} ${f(y)} ${f(z)};`);
      },
      rmAll(x, y, z) {
        const f = x => (typeof x === 'string' ? JSON.stringify(x) : (x.bound = 1, x.name));
        commands.push(`rmAll ${f(x)} ${f(y)} ${f(z)};`);
      },
      find1(x, y, z) {
        const f = x => (typeof x === 'string' ? JSON.stringify(x) : (x.bound = 1, x.name));
        commands.push(`find1 ${f(x)} ${f(y)} ${f(z)};`);
      },
      findAll(x, y, z) {
        const f = x => (typeof x === 'string' ? JSON.stringify(x) : (x.bound = 1, x.name));
        commands.push(`findAll ${f(x)} ${f(y)} ${f(z)};`);
      },
      collect(f) {
        const params = f.toString().split('=>')[0].match(/[\w\d]+/g);
        const env_ = env;
        commands.push(`collect "${collectCount}" ${params.join(' ')};`);
        collectCb['"' + collectCount + '"'] = f;
      }
    });
    const res = await this.sendRawQuery(commands.join('\n'));
    res.map(x => collectCb[x[0]](...x.slice(1)));
    return res; // TODO
  }

}
/*
query(yami => {
  const {x, y, z} = yami.common;
  const {a, b, c} = yami.symbol;
  const {d, e, f} = yami.locked;
  const {g, h} = yami.var;

  yami.find(x, y, z);
  yami.collect(x, y);
})

query.string(); // => 'common a, b, c;'

query(x => {
  const v = x.var.v;
  x.add(x.common.root, x.common.has, v);
  x.collect(v => );
});

*/
