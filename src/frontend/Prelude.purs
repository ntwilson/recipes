module Frontend.Prelude (module Exports, exceptToWidget) where

import Prelude

import Affjax.Web (defaultRequest, printError, request) as Exports
import Concur.Core (Widget(..), WidgetStep(..)) as Exports
import Concur.React (HTML) as Exports
import Concur.React.DOM (El, El', El1, ElLeaf, ElLeaf', ElLeafFunc', _data, _data', _data_, _map, _map', _map_, a, a', a_, abbr, abbr', abbr_, address, address', address_, area, area', article, article', article_, aside, aside', aside_, audio, audio', audio_, b, b', b_, base, base', bdi, bdi', bdi_, bdo, bdo', bdo_, big, big', big_, blockquote, blockquote', blockquote_, body, body', body_, br, br', button, button', button_, canvas, canvas', canvas_, caption, caption', caption_, cite, cite', cite_, code, code', code_, col, col', colgroup, colgroup', colgroup_, datalist, datalist', datalist_, dd, dd', dd_, del, del', del_, details, details', details_, dfn, dfn', dfn_, dialog, dialog', dialog_, div, div', div_, dl, dl', dl_, dt, dt', dt_, em, em', em_, embed, embed', fieldset, fieldset', fieldset_, figcaption, figcaption', figcaption_, figure, figure', figure_, footer, footer', footer_, form, form', form_, h1, h1', h1_, h2, h2', h2_, h3, h3', h3_, h4, h4', h4_, h5, h5', h5_, h6, h6', h6_, head, head', head_, header, header', header_, hr, hr', html, html', html_, i', i_, iframe, iframe', iframe_, img, img', input, input', ins, ins', ins_, int, kbd, kbd', kbd_, keygen, keygen', label, label', label_, legend, legend', legend_, li, li', li_, link, link', main, main', main_, mark, mark', mark_, menu, menu', menu_, menuitem, menuitem', meta, meta', meter, meter', meter_, nav, nav', nav_, noscript, noscript', noscript_, number, object, object', object_, ol, ol', ol_, optgroup, optgroup', optgroup_, option, option', option_, output, output', output_, p, p', p_, param, param', picture, picture', picture_, pre, pre', pre_, progress, progress', progress_, q, q', q_, rp, rp', rp_, rt, rt', rt_, ruby, ruby', ruby_, s, s', s_, samp, samp', samp_, script, script', script_, section, section', section_, select, select', select_, small, small', small_, source, source', span, span', span_, strong, strong', strong_, style, style', style_, sub', sub_, summary, summary', summary_, sup, sup', sup_, table, table', table_, tbody, tbody', tbody_, td, td', td_, text, textarea, textarea', textarea_, tfoot, tfoot', tfoot_, th, th', th_, thead, thead', thead_, time, time', time_, title, title', title_, tr, tr', tr_, track, track', u, u', u_, ul, ul', ul_, var, var', var_, video, video', video_, viewAdapter, wbr, wbr') as Exports
import Control.Alt (class Alt, alt, (<|>)) as Exports
import Control.Monad.Error.Class (throwError)
import Effect.Aff (error)
import Effect.Aff.Class (liftAff)
import Effect.Class.Console (log)
import Shared.Prelude hiding (div) as Exports
import Control.Monad.Except.Checked (handleErrors)

exceptToWidget :: ∀ r a. Monoid r => Exports.ExceptV (Exports.STRING_ERROR ()) (Exports.Widget r) a -> Exports.Widget r a
exceptToWidget = handleErrors { stringError: \err -> log err *> liftAff (throwError (error err)) }

