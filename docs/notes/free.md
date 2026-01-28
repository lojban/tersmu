# Frees (and indicators)

**Assumption** (backed up by 6.11): the meaning and effect of a free is independent of its position in a statement.

(So things which may sound natural in English like  
`mi prami do doi djan. gi'e xebni do doi bab.`  
are not actually supported. No great loss; I can't find any examples in the bpfk corpus.)

Although it's wrong, for now we can treat indicators the same way.

So we can preparse to bring all frees to the starts of statement. Although actually, that's non-trivial in itself, and seems to need a full parser… perhaps piggy-backing on jbofihe would be best for that?

## Or we could do it like this

**fold on text:**

- parse a Statement, defreeing on fail
- return Statement and remaining text

**to defree:**

- try to parse from point of failure: `free+`
- on fail: if failpoint is start, fail; else, defree
- return the Frees to be appended to list associated with the Statement, and remaining text to be appended to text for reparse.

Note that we are going to need a more complicated system if we want to properly handle UI with logical force like `{ei}` and `{da'i}` — we will need to associate them with the correct, possibly embedded, statement/subsentence.

There's also `fu'e` to complicate matters further.

## Actually, a small modification

On finding a free, rather than just reparsing the stripped parent, first insert a **magic token** at the start of the statement/fragment/subsentence immediately preceding the free, with this token referring by index to the free (the token could be of the form `"^37"`).

To find the start of a statement/fragment/subsentence: ignoring all quotes, look for NU NAI? / NOI / TO / I / NIhO / [startOfString].

Then we need to add to the start of statement, fragment, and free, a rule to match the magic tokens, which are looked up in the master list.

Note with this approach, we can revert to using the text production for the main parse.

Those indicators with proper grammatical functions, like `{kau}`, should be parsed in the main grammar (and so won't give rise to parse errors, so won't trigger the special handling above).
