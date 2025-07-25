#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "memory.h"
#include "object.h"
#include "value.h"

#include "table.h"

#define TABLE_MAX_LOAD 0.75

void initTable(Table *table)
{
    table->capacity = -1;
    table->count = 0;
    table->entries = NULL;
}

void freeTable(Table *table)
{
    FREE_ARRAY(Entry, table->entries, table->capacity+1);
    initTable(table);
}

Entry *findEntry(Entry *entries, int capacity, ObjString *key)
{
    uint32_t index = key->hashCode & capacity;
    Entry *tombstone = NULL;

    for (;;)
    {
        Entry *entry = &entries[index];

        if (entry->key == NULL)
        {
            if (IS_NULL(entry->value))
            {
                return tombstone != NULL ? tombstone : entry;
            }
            else
            {
                if (tombstone == NULL)
                {
                    tombstone = entry;
                }
            }
        }
        else if (entry->key == key)
        {
            return entry;
        }
        index = (index + 1) & capacity;
    }
}

void adjustCapacity(Table *table, int capacity)
{
    Entry *entries = ALLOCATE(Entry, capacity+1);
    for (int i = 0; i <= capacity; i++)
    {
        entries[i].key = NULL;
        entries[i].value = NULL_VAL;
    }

    table->count = 0;
    for (int i = 0; i <= table->capacity; i++)
    {
        Entry *entry = &table->entries[i];
        if (entry->key == NULL)
            continue;

        Entry *dest = findEntry(entries, capacity, entry->key);
        dest->key = entry->key;
        dest->value = entry->value;
        table->count++;
    }

    FREE_ARRAY(Entry, table->entries, table->capacity+1);

    table->entries = entries;
    table->capacity = capacity;
}

bool tableSet(Table *table, ObjString *key, Value value)
{
    if ((table->count + 1) > ((table->capacity+1) * TABLE_MAX_LOAD))
    {
        int capacity = GROW_CAPACITY(table->capacity+1)-1;
        adjustCapacity(table, capacity);
    }
    Entry *entry = findEntry(table->entries, table->capacity, key);

    bool isNewKey = entry->key == NULL;
    if (isNewKey && IS_NULL(entry->value))
        table->count++;

    entry->key = key;
    entry->value = value;
    return isNewKey;
}

void tableAddAll(Table *from, Table *to)
{
    for (int i = 0; i <= from->capacity; i++)
    {
        Entry *entry = &from->entries[i];
        if (entry->key != NULL)
        {
            tableSet(to, entry->key, entry->value);
        }
    }
}

bool tableGet(Table *table, ObjString *key, Value *value)
{
    if (table->count == 0)
        return false;

    Entry *entry = findEntry(table->entries, table->capacity, key);
    if (entry->key == NULL)
        return false;

    *value = entry->value;
    return true;
}

bool tableDel(Table *table, ObjString *key)
{
    if (table->count == 0)
        return false;

    Entry *entry = findEntry(table->entries, table->capacity, key);
    if (entry->key == NULL)
        return false;

    entry->key == NULL;
    entry->value = BOOL_VAL(true); // tombstone

    return true;
}

ObjString *tableFindString(Table *table, const char *chars, int length, uint32_t hash)
{
    if (table->count == 0)
    {
        return NULL;
    }

    uint32_t index = hash & table->capacity;

    for (;;)
    {
        Entry *entry = &table->entries[index];

        if (entry->key == NULL)
        {
            if (IS_NULL(entry->value))
                return NULL; // stop on empty non tombstone
        }
        else if (entry->key->length == length && entry->key->hashCode == hash &&
                 memcmp(entry->key->chars, chars, length) == 0)
        {
            return entry->key;
        }
        index = (index + 1) & table->capacity;
    }
}

void markTable(Table* table) {
    for(int i=0; i<=table->capacity; i++) {
        Entry* ent = &table->entries[i];
        markObject((Obj*)ent->key);
        markValue(ent->value);
    }
}


void tableRemoveWhite(Table* table) {
    for(int i=0; i<=table->capacity; i++) {
        Entry* ent = &table->entries[i];
        if(ent->key != NULL && !ent->key->obj.isMarked) {
            tableDel(table, ent->key);
        }
    }
}