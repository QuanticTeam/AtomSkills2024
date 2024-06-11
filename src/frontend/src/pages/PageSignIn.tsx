import { Typography } from 'antd'
import { useContext } from 'react'
import { useTranslation } from 'react-i18next'
import { Link } from 'react-router-dom'
import { UserApi } from '~/entities/User'
import { PageUnauthorized } from '~/layouts/PageUnauthorized'
import { AuthContext } from '~/shared/auth'
import { ROUTE_PATH_SIGN_UP } from '~/shared/routng/constants'
import { FormSignIn } from '~/widgets/FormSignIn'

export default function PageSignIn() {
  const { login } = useContext(AuthContext)
  const { t } = useTranslation()

  return (
    <PageUnauthorized>
      <FormSignIn
        onSubmit={async dto => {
          login(await UserApi.signIn(dto))
        }}
      >
        <Typography.Text>
          {t('or')} <Link to={ROUTE_PATH_SIGN_UP}>{t('Sign up')}</Link>
        </Typography.Text>
      </FormSignIn>
    </PageUnauthorized>
  )
}
