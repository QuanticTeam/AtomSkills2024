import { App, Typography } from 'antd'
import { useEffect, useState } from 'react'
import { useTranslation } from 'react-i18next'
import { Link, useNavigate } from 'react-router-dom'
import { UserApi } from '~/entities/User'
import { UserRole, UserRoleApi } from '~/entities/UserRole'
import { PageUnauthorized } from '~/layouts/PageUnauthorized'
import { ROUTE_PATH_SIGN_IN } from '~/shared/routng/constants'
import { FormSignUp } from '~/widgets/FormSignUp'

export default function PageSignUp() {
  const navigate = useNavigate()
  const { t } = useTranslation()
  const { message } = App.useApp()
  const [roles, setRoles] = useState<UserRole[]>([])

  useEffect(() => {
    async function fetchRoles() {
      setRoles(await UserRoleApi.fetchUserRoles())
    }

    fetchRoles()
  }, [])

  return (
    <PageUnauthorized>
      <FormSignUp
        onSubmit={async dto => {
          await UserApi.signUp(dto)

          message.info(t('Successfuly signed up'), 10)

          navigate(ROUTE_PATH_SIGN_IN)
        }}
        roles={roles}
      >
        <Typography.Text>
          {t('or')} <Link to={ROUTE_PATH_SIGN_IN}>{t('Sign in')}</Link>
        </Typography.Text>
      </FormSignUp>
    </PageUnauthorized>
  )
}
