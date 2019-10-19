<template>
  <div class="component">
    <div>
      <button @click="fetch">fetch</button>
      signify: {{ signify }}
    </div>
    <div>
      schemas
      <div v-for="(schema, index) in schemas" :key="index" @click="fetchSchema(schema)"
           class="list-item">
        {{ schema[0] }}
      </div>
    </div>
    <div>
      schema: {{ (currentSchema || ['-'])[0] }}
      <div v-for="(attribute, index) in schemaAttributes" :key="index"
           class="list-item">
        {{ attribute[0] }}
      </div>
    </div>
    <div>
      entities
      <div v-for="(entity, index) in entities" :key="index"
           class="list-item">
        {{ entity[0] }}
      </div>
    </div>
  </div>
</template>

<script>
import {yami} from '../yami'

export default {
  name: 'TableView',
  props: [],
  components: {
  },
  data() {
    return {
      signify: null,
      schemas: [],
      currentSchema: null,
      schemaAttributes: [],
      entities: []
    }
  },
  methods: {
    async fetch() {
      await yami.query(s => {
        const {signify} = s.var
        s.find1(signify, "signify", signify)
        s.collect(signify => {
          this.signify = signify
        })
      })
      const signify = this.signify
      this.schemas = []
      await yami.query(s => {
        const {schema, isa, x, target, targetName} = s.var
        s.find1(signify, "schema", schema)
        s.find1(signify, "isa", isa)
        s.findAll(isa, x, schema)
        s.find1(schema, target, x)
        s.find1(signify, targetName, target)
        s.collect((targetName, target) => {
          this.schemas.push([targetName, target])
        })
      })
    },
    async fetchSchema(schema) {
      this.currentSchema = schema
      this.schemaAttributes = []
      this.entities = []
      const signify = this.signify
      await yami.query(s => {
        const {attribute, attr, attrName} = s.var
        s.find1(signify, "attribute", attribute)
        s.findAll(attribute, schema[1], attr)
        s.find1(signify, attrName, attr)
        s.collect((attrName, attr) => {
          this.schemaAttributes.push([attrName, attr])
        })
      })
      await yami.query(s => {
        const {isa, target, targetName} = s.var
        s.find1(signify, "isa", isa)
        s.findAll(isa, target, schema[1])
        s.find1(signify, targetName, target)
        s.collect((targetName, target) => {
          this.entities.push([targetName, target])
        })
      })
    }
  }
}
</script>

<!-- Add "scoped" attribute to limit CSS to this component only -->
<style scoped>
.component {
  display: grid;
  grid-template-rows: 30px 1fr;
  grid-template-columns: 1fr 1fr 3fr;
}

.component > div:nth-child(1) {
  grid-row: 1;
  grid-column: 1 / 4;
}

.component > div:nth-child(2) {
  grid-row: 2;
  grid-column: 1;
}

.component > div:nth-child(3) {
  grid-row: 2;
  grid-column: 2;
}

.component > div:nth-child(4) {
  grid-row: 2;
  grid-column: 3;
}

.list-item {
  background: #ddd;
}
</style>
