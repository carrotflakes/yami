export class Client {
  private uri: string;

  constructor(uri: string) {
    this.uri = uri;
  }

  
}

export type Relation = {
  label: Symbol | String,
  from: Symbol | String,
  to: Symbol | String,
};

export class Symbol {
  index: number;
  relations: Relation[];

  constructor(index: number) {
    this.index = index;
    this.relations = [];
  }
}

export class String {
  str: string;
  relations: Relation[];

  constructor(str: string) {
    this.str = str;
    this.relations = [];
  }
}

export class Repository {
  nodes: (Symbol | String)[];

  constructor() {
    this.nodes = [];
  }

  addNode(node: Symbol | String) {
    this.nodes.push(node);
  }

  allNodes() {
    return this.nodes;
  }
}
