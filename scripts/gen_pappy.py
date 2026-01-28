#!/usr/bin/env python3
"""
Generate .pappy from canonical .pest + .pappy.rhs (types and RHS per rule).

Usage: gen_pappy.py <grammar.pest> <grammar.pappy.rhs> -o <grammar.pappy>

Reads rule names in order from .pest, looks up type and RHS from .pappy.rhs,
outputs a .pappy file that pappy can compile (same as before, but derived from pest).
"""

import re
import sys
import argparse
from pathlib import Path


def read_pest_rules(pest_path: str):
    """Return list of rule names in order of appearance in .pest file."""
    with open(pest_path, 'r', encoding='utf-8') as f:
        content = f.read()
    rules = []
    for line in content.split('\n'):
        line = line.strip()
        if line.startswith('//') or not line:
            continue
        m = re.match(r'^([a-zA-Z_][a-zA-Z0-9_]*)\s*=\s*\{', line)
        if m:
            rules.append(m.group(1))
    return rules


def read_rhs_file(rhs_path: str):
    """Return (header_str, dict of rule_name -> (type, rhs_str))."""
    with open(rhs_path, 'r', encoding='utf-8') as f:
        content = f.read()
    if '---RULES---' not in content:
        raise ValueError('No ---RULES--- marker in ' + rhs_path)
    header_part, rules_part = content.split('---RULES---', 1)
    if header_part.strip().startswith('HEADER:'):
        header_part = header_part.replace('HEADER:', '', 1).strip()
    header = header_part.strip()
    rule_dict = {}
    for block in re.split(r'\n\nRULE:', rules_part):
        block = block.strip()
        if not block:
            continue
        lines = block.split('\n')
        # First line is either "RULE:name" or just "name" (after split)
        name_line = lines[0].strip()
        if name_line.startswith('RULE:'):
            rule_name = name_line[5:].strip()
        else:
            rule_name = name_line
        type_line = None
        rhs_start = None
        rhs_first_from_type = None  # when TYPE line contains " = rhs...", RHS got merged
        for i, line in enumerate(lines[1:], 1):
            if line.startswith('TYPE:'):
                rest = line[5:].strip()
                # TYPE line can contain " = rhs..." or "\t= rhs..." if RHS was merged (e.g. Space rule)
                parts = re.split(r'\s+=\s+', rest, maxsplit=1)
                if len(parts) == 2:
                    type_line, rhs_first_from_type = parts[0].strip(), parts[1].strip()
                else:
                    type_line = rest
            if line.strip() == 'RHS:':
                rhs_start = i + 1
        if type_line is None or rhs_start is None:
            continue
        rhs_lines = lines[rhs_start:]
        if rhs_first_from_type is not None:
            rhs_lines = [rhs_first_from_type] + rhs_lines
        rhs_str = '\n'.join(rhs_lines).strip()
        rule_dict[rule_name] = (type_line, rhs_str)
    return header, rule_dict


def main():
    ap = argparse.ArgumentParser(description='Generate .pappy from .pest + .pappy.rhs')
    ap.add_argument('pest', help='Input .pest file (defines rule order)')
    ap.add_argument('rhs', help='Input .pappy.rhs file (header, types, RHS)')
    ap.add_argument('-o', '--output', required=True, help='Output .pappy file')
    args = ap.parse_args()
    rule_order = read_pest_rules(args.pest)
    header, rule_dict = read_rhs_file(args.rhs)
    with open(args.output, 'w', encoding='utf-8') as f:
        f.write(header)
        f.write('\n\n')
        for rule_name in rule_order:
            if rule_name not in rule_dict:
                sys.stderr.write('Warning: rule %s in .pest not found in .pappy.rhs\n' % rule_name)
                continue
            typ, rhs = rule_dict[rule_name]
            lines = rhs.split('\n') if rhs else []
            # RHS in .pappy.rhs often starts with " = "; strip one " = " when outputting
            def strip_leading_equals(s):
                s = s.strip()
                if s.startswith(' = '):
                    return s[3:].strip()
                if s.startswith('='):
                    return s[1:].strip()
                return s
            if len(lines) <= 1:
                first = strip_leading_equals(lines[0] if lines else '')
                if first:
                    f.write(rule_name + ' :: ' + typ + ' = ' + first + '\n')
            else:
                # Skip leading comment-only lines so first alternative is valid (pappy rejects " = comment" as alt)
                start = 0
                while start < len(lines) and lines[start].strip().startswith('--'):
                    f.write(lines[start] + '\n')
                    start += 1
                if start >= len(lines):
                    f.write(rule_name + ' :: ' + typ + '\n\n')
                    continue
                first = strip_leading_equals(lines[start])
                f.write(rule_name + ' :: ' + typ + '\n')
                f.write('    = ' + first + '\n')
                for line in lines[start + 1:]:
                    # In pappy only the first RHS line starts with " = "; further alternatives use " / "
                    if line.strip().startswith('=') and not line.strip().startswith('/'):
                        line = '    / ' + line.strip()[1:].lstrip()
                    f.write(line + '\n')
            f.write('\n')
    print('Wrote', args.output, '(%d rules)' % len(rule_order))


if __name__ == '__main__':
    main()
