import { App, Typography } from 'antd'
import { useEffect, useState } from 'react'
import { Link, useNavigate } from 'react-router-dom'
import { UserApi } from '~/entities/User'
import { UserRole, UserRoleApi } from '~/entities/UserRole'
import { PageUnauthorized } from '~/layouts/PageUnauthorized'
import { FormRegister } from '~/widgets/FormRegister'

export default function PageRegister() {
  const navigate = useNavigate()
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
      <FormRegister
        onSubmit={async dto => {
          await UserApi.register(dto)

          message.info('Вы успешно зарегистрированы', 10) // TODO t

          navigate('/login')
        }}
        roles={roles}
      >
        <Typography.Text>
          {/* TODO t */}
          или <Link to="/login">войти</Link>
        </Typography.Text>
      </FormRegister>
    </PageUnauthorized>
  )
}
