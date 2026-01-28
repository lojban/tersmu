#!/usr/bin/env python3
"""
Convert a .pappy grammar file to:
1. A .pest file (structure only, for canonical grammar / Rust port)
2. A .pappy.rhs file (types and RHS with actions, for Haskell codegen)

Usage: pappy_to_pest.py <input.pappy> [--pest <out.pest>] [--rhs <out.pappy.rhs>]
Default: writes <basename>.pest and <basename>.pappy.rhs next to input.
"""

import re
import sys
import os
import argparse
from pathlib import Path


def split_pappy_content(content: str):
    """
    Split pappy file into header (before first rule) and list of (rule_name, type, rhs).
    Header includes everything up to and including the closing } of "parser name: { ... }",
    plus the "top ..." line. Rules start with ruleName :: and are not inside the parser block.
    """
    lines = content.split('\n')
    i = 0
    header_lines = []
    # Collect until we're past the parser block
    while i < len(lines):
        line = lines[i]
        if re.match(r'^\s*parser\s+\w+\s*:', line):
            header_lines.append(line)
            i += 1
            while i < len(lines) and lines[i].strip() != '{':
                header_lines.append(lines[i])
                i += 1
            if i < len(lines):
                header_lines.append(lines[i])  # the "{"
                i += 1
            depth = 1
            while i < len(lines) and depth > 0:
                for c in lines[i]:
                    if c == '{':
                        depth += 1
                    elif c == '}':
                        depth -= 1
                header_lines.append(lines[i])
                i += 1
            break
        header_lines.append(line)
        i += 1
    # Skip "top ..." and comments/blanks; then collect rules
    rule_start_re = re.compile(r'^([a-zA-Z_][a-zA-Z0-9_]*)\s*::\s*(.*)$')
    rules = []
    while i < len(lines):
        line = lines[i]
        if line.strip() == '' or line.strip().startswith('--') or line.strip().startswith('top '):
            header_lines.append(line)
            i += 1
            continue
        m = rule_start_re.match(line)
        if m:
            rule_name = m.group(1)
            rest = m.group(2).rstrip()
            if ' = ' in rest:
                type_part, rhs_first = rest.split(' = ', 1)
                type_part = type_part.strip()
                rhs_lines = [rhs_first]
                i += 1
            else:
                type_part = rest.strip()
                rhs_lines = []
                i += 1
            while i < len(lines):
                cont = lines[i]
                stripped = cont.strip()
                if not stripped:
                    rhs_lines.append(cont)
                    i += 1
                    continue
                if stripped.startswith('--'):
                    rhs_lines.append(cont)
                    i += 1
                    continue
                if re.match(r'^\s+(?:=|\/)', cont):
                    rhs_lines.append(cont)
                    i += 1
                    continue
                if rule_start_re.match(cont):
                    break
                rhs_lines.append(cont)
                i += 1
            rhs_str = '\n'.join(rhs_lines).strip()
            rules.append((rule_name, type_part, rhs_str))
            continue
        header_lines.append(line)
        i += 1

    header = '\n'.join(header_lines)
    return header, rules


def item_to_pest(item: str) -> str:
    """Convert one pappy item to pest (strip binding name:, handle ! & "literal" rule ?*+)."""
    item = item.strip()
    if not item:
        return ''
    # Binding: name:rule or name:(...)
    m = re.match(r'^[a-zA-Z_][a-zA-Z0-9_]*\s*:\s*(.*)$', item)
    if m:
        item = m.group(1).strip()
    # Literal "..." - keep as-is for pest
    if item.startswith('"'):
        # Find closing quote (allow escaped)
        end = 1
        while end < len(item):
            if item[end] == '\\':
                end += 2
                continue
            if item[end] == '"':
                end += 1
                return item[:end]
            end += 1
        return item
    # !rule
    if item.startswith('!'):
        inner = item_to_pest(item[1:].strip())
        return '!' + inner if inner else '!' + item[1:]
    # &rule (not &{ ... })
    if item.startswith('&') and not item.startswith('&{'):
        inner = item_to_pest(item[1:].strip())
        return '&' + inner if inner else '&' + item[1:]
    # &{ ... } - semantic predicate; drop or use comment
    if item.startswith('&{'):
        return ''
    # rule?, rule*, rule+
    for suffix in ('?', '*', '+'):
        if item.endswith(suffix) and len(item) > 1:
            inner = item[:-1].strip()
            return (item_to_pest(inner) if inner else inner) + suffix
    # ( ... ) group - may contain / for choice; recurse with full RHS conversion
    if item.startswith('('):
        depth = 0
        for j, c in enumerate(item):
            if c == '(':
                depth += 1
            elif c == ')':
                depth -= 1
                if depth == 0:
                    inner = item[1:j].strip()
                    return '(' + rule_rhs_to_pest(inner) + ')'
        return item
    # Plain rule or token
    return item


def split_alt_rhs(rhs: str):
    """Split RHS into alternatives by / (not inside (), {}, "")."""
    alts = []
    current = []
    depth_p = 0
    depth_b = 0
    in_str = False
    i = 0
    while i < len(rhs):
        c = rhs[i]
        if in_str:
            if c == '\\':
                current.append(rhs[i:i+2])
                i += 2
                continue
            if c == '"':
                in_str = False
            current.append(c)
            i += 1
            continue
        if c == '"':
            in_str = True
            current.append(c)
            i += 1
            continue
        if c == '(':
            depth_p += 1
            current.append(c)
            i += 1
            continue
        if c == ')':
            depth_p -= 1
            current.append(c)
            i += 1
            continue
        if c == '{':
            depth_b += 1
            current.append(c)
            i += 1
            continue
        if c == '}':
            depth_b -= 1
            current.append(c)
            i += 1
            continue
        if c == '/' and depth_p == 0 and depth_b == 0:
            nxt = i + 1
            while nxt < len(rhs) and rhs[nxt] in ' \t\n':
                nxt += 1
            if nxt < len(rhs) and rhs[nxt] not in '':
                alts.append(''.join(current).strip())
                current = []
            i = nxt
            continue
        current.append(c)
        i += 1
    if current:
        alts.append(''.join(current).strip())
    return alts


def strip_action(alt: str) -> str:
    """Remove trailing -> { ... } from an alternative (balance braces)."""
    idx = alt.rfind(' -> ')
    if idx == -1:
        return alt.strip()
    rest = alt[idx+4:].lstrip()
    if not rest.startswith('{'):
        return alt[:idx].strip()
    depth = 0
    for j, c in enumerate(rest):
        if c == '{':
            depth += 1
        elif c == '}':
            depth -= 1
            if depth == 0:
                return alt[:idx].strip()
    return alt[:idx].strip()


def split_seq(alt: str):
    """Split one alternative into sequence items (by space, not inside () {} "")."""
    items = []
    current = []
    depth_p = 0
    depth_b = 0
    in_str = False
    i = 0
    alt = alt.strip()
    while i < len(alt):
        c = alt[i]
        if in_str:
            if c == '\\':
                current.append(alt[i:i+2])
                i += 2
                continue
            if c == '"':
                in_str = False
            current.append(c)
            i += 1
            continue
        if c == '"':
            in_str = True
            current.append(c)
            i += 1
            continue
        if c in ' \t\n':
            if depth_p == 0 and depth_b == 0 and current:
                item = ''.join(current).strip()
                if item:
                    items.append(item)
                current = []
            elif current:
                current.append(c)
            i += 1
            continue
        if c == '(':
            depth_p += 1
            current.append(c)
            i += 1
            continue
        if c == ')':
            depth_p -= 1
            current.append(c)
            i += 1
            continue
        if c == '{':
            depth_b += 1
            current.append(c)
            i += 1
            continue
        if c == '}':
            depth_b -= 1
            current.append(c)
            i += 1
            continue
        current.append(c)
        i += 1
    if current:
        item = ''.join(current).strip()
        if item:
            items.append(item)
    return items


def seq_to_pest(alt: str) -> str:
    """Convert one alternative (sequence) to pest: items joined by ~."""
    alt_clean = strip_action(alt)
    items = split_seq(alt_clean)
    pest_items = [item_to_pest(it) for it in items if item_to_pest(it)]
    return ' ~ '.join(pest_items) if pest_items else '""'


def rule_rhs_to_pest(rhs: str) -> str:
    """Convert full rule RHS (all alternatives) to pest rule body."""
    alts = split_alt_rhs(rhs)
    pest_alts = []
    for alt in alts:
        alt = alt.strip()
        if not alt:
            continue
        # Strip leading " = " from first alternative (pappy continuation style)
        if alt.startswith(' = '):
            alt = alt[3:].strip()
        elif alt.startswith('='):
            alt = alt[1:].strip()
        p = seq_to_pest(alt)
        if p:
            pest_alts.append(p)
    return ' | '.join(pest_alts) if pest_alts else '""'


def main():
    ap = argparse.ArgumentParser(description='Convert .pappy to .pest and extract RHS')
    ap.add_argument('input', help='Input .pappy file')
    ap.add_argument('--pest', help='Output .pest file')
    ap.add_argument('--rhs', help='Output .pappy.rhs file (types and RHS per rule)')
    args = ap.parse_args()
    base = Path(args.input).resolve()
    with open(args.input, 'r', encoding='utf-8') as f:
        content = f.read()
    header, rules = split_pappy_content(content)
    pest_out = args.pest or str(Path(args.input).with_suffix('.pest'))
    rhs_out = args.rhs or str(Path(args.input).with_suffix('.pappy.rhs'))
    # Write .pest
    with open(pest_out, 'w', encoding='utf-8') as f:
        f.write("// Generated from " + os.path.basename(args.input) + " - structure only\n")
        f.write("// Pest grammar - canonical source for Rust port\n\n")
        for name, _type, rhs in rules:
            body = rule_rhs_to_pest(rhs)
            f.write(name + " = { " + body + " }\n\n")
    # Write .pappy.rhs: header + per-rule type and full RHS
    with open(rhs_out, 'w', encoding='utf-8') as f:
        f.write("HEADER:\n")
        f.write(header)
        f.write("\n\n---RULES---\n\n")
        for name, typ, rhs in rules:
            f.write("RULE:" + name + "\n")
            f.write("TYPE:" + typ + "\n")
            f.write("RHS:\n")
            f.write(rhs)
            f.write("\n\n")
    print("Wrote", pest_out, "and", rhs_out, "(%d rules)" % len(rules))


if __name__ == '__main__':
    main()
