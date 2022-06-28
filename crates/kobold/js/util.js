export function __kobold_start(n) { document.body.appendChild(n); }

export function __kobold_append(n,c) { n.appendChild(c); }
export function __kobold_unmount(n) { n.remove(); }
export function __kobold_replace(o,n) { o.replaceWith(n); }
export function __kobold_empty_node() { return document.createTextNode(""); }
export function __kobold_fragment()
{
	let f = document.createDocumentFragment();
	f.append(f.$begin = document.createTextNode(""), f.$end = document.createTextNode(""));
	return f;
};
export function __kobold_fragment_decorate(f) {
	f.$begin = f.firstChild;
	f.$end = f.lastChild;
}
export function __kobold_fragment_append(f,c) { f.$end.before(c); }
export function __kobold_fragment_unmount(f)
{
	let b = f.$begin, e = f.$end;
	while (b.nextSibling !== e) f.appendChild(b.nextSibling);
	f.appendChild(e);
	f.insertBefore(b, f.firstChild);
}
export function __kobold_fragment_replace(f,n)
{
	let b = f.$begin, e = f.$end;
	while (b.nextSibling !== e) f.appendChild(b.nextSibling);
	b.replaceWith(n);
	f.appendChild(e);
	f.insertBefore(b, f.firstChild);
}
export function __kobold_fragment_drop(f)
{
	delete f.$begin;
	delete f.$end;
}
export function __kobold_text_node(t) { return document.createTextNode(t); }
export function __kobold_update_text(n,t) { n.textContent = t; }
export function __kobold_create_div() { return document.createElement('div'); }

export function __kobold_attr(n,v) { let a = document.createAttribute(n); a.value = v; return a; }
export function __kobold_attr_class(v) { let a = document.createAttribute('class'); a.value = v; return a; }
export function __kobold_attr_style(v) { let a = document.createAttribute('style'); a.value = v; return a; }
export function __kobold_attr_set(n,k,v) { n.setAttribute(k, v); }
export function __kobold_attr_update(n,v) { n.value = v; }

export function __kobold_attr_checked_set(n,v) { if (n.checked !== v) n.checked = v; }
export function __kobold_class_add(n,v) { n.classList.add(v); }
export function __kobold_class_remove(n,v) { n.classList.remove(v); }
export function __kobold_class_replace(n,o,v) { n.classList.replace(o,v); }
