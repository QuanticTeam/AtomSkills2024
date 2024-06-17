import i18next from 'i18next'
import { PageSignIn } from './PageSignIn'

import en from './localization/en.json'
import ru from './localization/ru.json'

i18next.addResourceBundle('en', PageSignIn.name, en)
i18next.addResourceBundle('ru', PageSignIn.name, ru)

export default PageSignIn
