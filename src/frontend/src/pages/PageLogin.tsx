import { Typography } from 'antd'
import { useContext } from 'react'
import { Link } from 'react-router-dom'
import { UserApi } from '~/entities/User'
import { PageUnauthorized } from '~/layouts/PageUnauthorized'
import { AuthContext } from '~/shared/auth'
import { FormLogin } from '~/widgets/FormLogin'

export default function PageLogin() {
  const { login } = useContext(AuthContext)

  return (
    <PageUnauthorized>
      <FormLogin
        onSubmit={async dto => {
          login(await UserApi.login(dto))
        }}
      >
        <Typography.Text>
          {/* TODO t */}
          или <Link to="/register">зарегистрироваться</Link>
        </Typography.Text>
      </FormLogin>
    </PageUnauthorized>
  )
}
