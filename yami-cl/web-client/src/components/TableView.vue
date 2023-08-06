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
        {{ attribute[0] === 0 ? 'forward' : 'backward' }} : {{ attribute[1] }}
      </div>
    </div>
    <div>
      entities
      <!--div v-for="(entity, index) in entities" :key="index"
           class="list-item">
        {{ entity[0] }}
        </div-->
      <vue-good-table :columns="entitiesColumns" :rows="entitiesRows"/>
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
      entities: [],
      entitiesColumns: [],
      entitiesRows: []
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
        const {schema, isa, target, targetName} = s.var
        s.find1(signify, "schema", schema)
        s.find1(signify, "isa", isa)
        s.findAll(isa, target, schema)
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
        const {attributeF, attr, attrName} = s.var
        s.find1(signify, "attributeForward", attributeF)
        s.findAll(attributeF, schema[1], attr)
        s.find1(signify, attrName, attr)
        s.collect((attrName, attr) => {
          this.schemaAttributes.push([0, attrName, attr])
        })
      })
      await yami.query(s => {
        const {attributeB, attr, attrName} = s.var
        s.find1(signify, "attributeBackward", attributeB)
        s.findAll(attributeB, schema[1], attr)
        s.find1(signify, attrName, attr)
        s.collect((attrName, attr) => {
          this.schemaAttributes.push([1, attrName, attr])
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
      this.entitiesColumns = this.schemaAttributes.map(a => ({
        label: a[1].name,
        field: x => x[a[2].id]
      }))
      this.entitiesColumns.unshift({
        label: 'name',
        field: '_name'
      })
      const entitiesRows = this.entities.map(e => ({
        _name: e[0].name,
        _entity: e[1]
      }))
      for (const entity of entitiesRows) {
        for (const attribute of this.schemaAttributes) {
          if (attribute[0] === 0) {
            await yami.query(s => {
              const {target, targetName} = s.var
              s.findAll(attribute[2], entity._entity, target)
              s.find1(signify, targetName, target)
              s.collect((targetName, target) => {
                entity[attribute[2].id] = targetName.name
              })
            })
          } else {
            await yami.query(s => {
              const {target, targetName} = s.var
              s.findAll(attribute[2], target, entity._entity)
              s.find1(signify, targetName, target)
              s.collect((targetName, target) => {
                entity[attribute[2].id] = targetName.name
              })
            })
          }
        }
      }
      this.entitiesRows = entitiesRows
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
