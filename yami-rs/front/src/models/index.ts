export class Node {
  public type: 'str' | 'var' | 'sym' = 'str';
  public index: number | null = null;
  public string: string = '';
  public rep: string = '';

  constructor(str: string) {
    if (str[0] === '"') {
      this.type = 'str';
      this.string = JSON.parse(str);
    } else if (str[0] === ':') {
      this.type = 'sym';
      this.index = +str.slice(1);
    } else {
      this.type = 'var';
      this.string = str;
    }
    this.rep = str;
  }
}
